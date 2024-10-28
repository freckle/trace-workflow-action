import * as core from "@actions/core";
import * as github from "@actions/github";
import * as dotenv from 'dotenv';

import * as instrumentation from './instrumentation';
import { getInputs } from "./inputs";
import { getTracer, inSpan, type tag } from "./trace";
import { GitHub } from "@actions/github/lib/utils";
import { readLines } from "./streaming";
import { init } from './instrumentation'

dotenv.config();

const { githubToken, githubOwner, githubRepo, githubRunId, exporters } = getInputs();
const oktokit = github.getOctokit(githubToken);

instrumentation.init(exporters);

async function run() {
  try {
    const jobTags: Record<string, tag[]> = {};

    // GHA jobs which we want to tag with extra metadata
    const needJobLogs = new Map<string, tagBuilder> ([
      [ "jenkins-ephemeral", getDeployEnvironmentTags ],
      [ "ephemeral-staging", getDeployEnvironmentTags ], // backwards compatible with old job name
      [ "deploy-staging", getDeployEnvironmentTags]
    ])

    const { data: run } = await oktokit.rest.actions.getWorkflowRun({
      owner: githubOwner,
      repo: githubRepo,
      run_id: githubRunId,
    });

    const {
      data: { jobs, total_count },
    } = await oktokit.rest.actions.listJobsForWorkflowRun({
      owner: githubOwner,
      repo: githubRepo,
      run_id: githubRunId,
      attempt_number: run.run_attempt,
      per_page: 100, // we always expect fewer
    });

    if (jobs.length === 0) {
      throw new Error("Run has no Jobs");
    }

    if (total_count > 100) {
      throw new Error("Run had more than 100 Jobs");
    }

    // some jobs can be tagged with extra metadata
    // for these jobs, we need to pull the job logs and parse them
    await Promise.all(jobs.map(async (job) => {
      const tagBuilder = needJobLogs.get(job.name);
      if (tagBuilder != null) {
        const tags = await generateTags(oktokit, job.id, tagBuilder);
        console.debug(`settings tags for ${job.name}`);
        // Set the extra tags to be attached to the span for a given job
        jobTags[job.name] = tags;
      }}
    ));

    const traceableRun = {
      name: run.run_attempt ? `${run.name} #${run.run_attempt}` : run.name,
      started_at: run.run_started_at,
      completed_at: jobs
        .map((job) => job.completed_at)
        .sort()
        .reverse()[1],
      status: run.status,
      conclusion: run.conclusion,
    };

    const tracer = getTracer();

    inSpan(tracer, traceableRun, undefined, () => {
      jobs.forEach((job) => {
        inSpan(tracer, job, jobTags[job.name], () => {
          (job.steps || []).forEach((step) => {
            inSpan(tracer, step);
          });
        });
      });
    });
  } catch (error) {
    if (error instanceof Error) {
      core.error(error);
      core.setFailed(error.message);
    } else if (typeof error === "string") {
      core.error(error);
      core.setFailed(error);
    } else {
      core.error("Non-Error exception");
      core.setFailed("Non-Error exception");
    }
  }
}

type tagBuilder = (response: Response) => Promise<tag[]>

async function generateTags(oktokit: InstanceType<typeof GitHub>, jobId: number, buildTags: tagBuilder): Promise<tag[]> {
  const response = await oktokit.request('GET /repos/{owner}/{repo}/actions/jobs/{job_id}/logs', {
    owner: githubOwner,
    repo: githubRepo,
    job_id: jobId,
    headers: {
      'X-GitHub-Api-Version': '2022-11-28'
    }
  })

  const url = response.url;

  if (!url) {
    console.warn(`No logs found for job ${jobId}`);
    return [];
  }

  return fetch(url).then(buildTags);
}

// Read the logs for a deploy-environment call and return a list of metadata
// tags corresponding to the components which got deployed
async function getDeployEnvironmentTags(response: Response): Promise<tag[]> {

  // helper function which gets the component name from a docker image tag or s3 uri
  function getArtifactName(log: string): string | null {
    const dockerArtifact = log.match(/amazonaws.com\/frontrow\/(?<artifact>[^:]+)/)?.groups?.artifact;
    const jsArtifact = log.match(/frontrow-artifacts\/(?<artifact>[^\/]+)/)?.groups?.artifact
    return dockerArtifact || jsArtifact || null;
  }

  const tags = [];
  let prev = '';

  // The relevant logs come in pairs:
  // * the first line identifies the deployable artifact
  // * the second line states whether or not there exists an artifact for the given SHA
  //
  // example:
  //
  // Verifying 853032795538.dkr.ecr.us-east-1.amazonaws.com/frontrow/fancy-api:869211fa8d815222eb36317f36016ff3618b3423
  //   ✗ image not found
  // Verifying s3://frontrow-artifacts/classroom/869211fa8d815222eb36317f36016ff3618b3423
  // ✓ bundle found
  if (response.body != null) {
    for await (const line of readLines(response.body.getReader())) {
      if (line.match(/✓ \w+ found/)) {
        const artifact = getArtifactName(prev);
        console.debug(`artifact name is ${artifact} for log ${prev}`)
        if (artifact != null) {
          tags.push({
            key: `deploy.${artifact}`,
            value: "true"
          })
        }
      }
      prev = line;
    }
  }
  return tags;
}

run();
