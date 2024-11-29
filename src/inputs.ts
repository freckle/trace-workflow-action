import * as core from "@actions/core";

export type Inputs = {
  githubToken: string;
  githubOwner: string;
  githubRepo: string;
  githubRunId: number;
};

export function getInputs(): Inputs {
  const rawRunId = core.getInput("github-run-id", { required: true });

  return {
    githubToken: core.getInput("github-token", { required: true }),
    githubOwner: core.getInput("github-owner", { required: true }),
    githubRepo: core.getInput("github-repo", { required: true }),
    githubRunId: parseInt(rawRunId, 10),
  };
}
