import * as core from "@actions/core";

export type Inputs = {
  githubToken: string;
  githubOwner: string;
  githubRepo: string;
  githubRunId: number;
};

function getInput(envVarName: string, inputName: string) {
  return process.env[envVarName] || core.getInput(inputName, { required: true });
}

export function getInputs(): Inputs {
  const rawRunId = getInput("GITHUB_RUN_ID", "github-run-id");

  return {
    githubToken: getInput("GITHUB_TOKEN", "github-token"),
    githubOwner: getInput("GITHUB_OWNER", "github-owner"),
    githubRepo: getInput("GITHUB_REPO", "github-repo"),
    githubRunId: parseInt(rawRunId, 10),
  };
}

