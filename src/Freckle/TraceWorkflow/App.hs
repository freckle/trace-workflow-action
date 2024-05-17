module Freckle.TraceWorkflow.App
  ( main

    -- * Extracted for testing
  , traceRunWithJobs
  ) where

import Relude

import Configuration.Dotenv qualified as Dotenv
import Freckle.TraceWorkflow.GitHub (RunWithJobs (..))
import Freckle.TraceWorkflow.GitHub qualified as GitHub
import Freckle.TraceWorkflow.OpenTelemetry
import Freckle.TraceWorkflow.WorkflowEnv
import UnliftIO (MonadUnliftIO)

main :: IO ()
main = do
  Dotenv.loadFile Dotenv.defaultConfig
  env <- getWorkflowEnv
  mRun <-
    GitHub.getRunWithJobs
      env.githubToken
      env.githubOwner
      env.githubRepo
      env.githubRunId

  traverse_ traceRunWithJobs mRun

traceRunWithJobs :: MonadUnliftIO m => RunWithJobs -> m ()
traceRunWithJobs run = do
  withTracer $ \tracer -> do
    recordSpan tracer run $ do
      for_ run.jobs $ \job -> do
        recordSpan tracer job $ do
          for_ job.steps $ \step -> do
            recordSpan_ tracer step
