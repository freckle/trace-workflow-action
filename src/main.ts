import * as core from "@actions/core";
import * as github from "@actions/github";
import * as dotenv from 'dotenv';

import { getInputs } from "./inputs";
import { getTracer, inSpan } from "./trace";

dotenv.config();

async function run() {
  try {
    const { githubToken, githubOwner, githubRepo, githubRunId } = getInputs();
    const oktokit = github.getOctokit(githubToken);

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

    inSpan(tracer, traceableRun, () => {
      jobs.forEach((job) => {
        inSpan(tracer, job, () => {
          (job.steps || []).forEach((step) => {
            console.debug(step)
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

run();
