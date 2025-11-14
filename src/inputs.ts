import * as core from "@actions/core";

export type Inputs = {
  githubToken: string;
  githubRepository: string;
  githubRunId: number;
  exporters: string[];
};

function getInput(envVarName: string, inputName: string) {
  return process.env[envVarName] || core.getInput(inputName, { required: true });
}

export function getInputs(): Inputs {
  const rawRunId = getInput("GITHUB_RUN_ID", "github-run-id");

  return {
    githubToken: getInput("GITHUB_TOKEN", "github-token"),
    githubRepository: getInput("GITHUB_REPOSITORY", "github-repository"),
    githubRunId: parseInt(rawRunId, 10),
    exporters: (getInput("OTEL_EXPORTERS", "otel-exporters") ?? 'collector').split(',') ,
  };
}

