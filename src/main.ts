import * as core from "@actions/core";
import * as dotenv from 'dotenv';
import * as fs from 'fs';
import * as github from "@actions/github";
import * as instrumentation from './instrumentation';

import { getInputs } from "./inputs";
import { getTracer, inSpan } from "./trace";
import { GitHub } from "@actions/github/lib/utils";

dotenv.config();

const { githubToken, githubRepository, githubRunId, exporters } = getInputs();
const [ githubOwner, githubRepo ] = githubRepository.split('/');
const oktokit = github.getOctokit(githubToken);

const provider = instrumentation.init(exporters);

async function fetchRunData(oktokit: InstanceType<typeof GitHub>) {
  console.debug(`fetching run data for run ${githubRunId}`);
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

  return { run, jobs };
}


async function run() {
  try {
    const { run, jobs } = await fetchRunData(oktokit);

    const traceableRun = {
      name: run.run_attempt ? `${run.name} #${run.run_attempt}` : run.name,
      started_at: run.run_started_at,
      completed_at: jobs
        .map((job: any) => job.completed_at)
        .sort()
        .reverse()[1],
      status: run.status,
      conclusion: run.conclusion,
    };

    const tracer = getTracer();

    const workflowSpan = inSpan(tracer, traceableRun);

    jobs.forEach((job: any) => {
      const jobSpan = inSpan(tracer, job, workflowSpan);
      (job.steps || []).forEach((step: any) => {
        inSpan(tracer, step, jobSpan);
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

  try {
    await provider.forceFlush();
    await provider.shutdown();
    console.debug('shutdown complete');
  } catch (error) {
    console.error('error shutting down provider', error);
  }
}


run();
