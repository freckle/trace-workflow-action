module Freckle.TraceWorkflow.App
  ( main
  ) where

import           Relude

import qualified Configuration.Dotenv                as Dotenv
import qualified Data.List.NonEmpty                  as NE
import qualified Data.List.NonEmpty.Extra            as NE
import           Freckle.TraceWorkflow.GitHub        (Job (..), Run (..),
                                                      Step (..), wasSkipped)
import qualified Freckle.TraceWorkflow.GitHub        as GitHub
import           Freckle.TraceWorkflow.OpenTelemetry
import           Freckle.TraceWorkflow.WorkflowEnv

main :: IO ()
main = do
  Dotenv.loadFile Dotenv.defaultConfig
  env <- getWorkflowEnv

  mJobs <-
    NE.nonEmpty
      . filter (not . wasSkipped)
      <$> GitHub.getJobs
        env.githubToken
        env.githubOwner
        env.githubRepo
        env.githubRunId

  for_ mJobs $ \jobs -> do
    run <-
      GitHub.getRun
        env.githubToken
        env.githubOwner
        env.githubRepo
        env.githubRunId

    let
      runSpanName = run.name -- TODO: attempt
      runSpanEnd = NE.maximum1 $ (.completed_at) <$> jobs

    withTracer $ \tracer -> do
      inSpan tracer runSpanName run.run_started_at Nothing $ do
        for_ jobs $ \job -> do
          inSpan tracer job.name job.started_at Nothing $ do
            for_ (filter (not . wasSkipped) job.steps) $ \step -> do
              inSpan tracer step.name step.started_at Nothing $ do
                pure step.completed_at
            pure job.completed_at
        pure runSpanEnd
