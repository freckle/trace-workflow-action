module Freckle.TraceWorkflow.App
  ( main
  ) where

import Relude

import Configuration.Dotenv qualified as Dotenv
import Data.Text (pack, unpack)
import OpenTelemetry.Trace
import System.Environment (getEnv)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket)
import Prelude qualified as Unsafe (read)

data WorkflowEnv = WorkflowEnv
  { githubCommitSha :: Text
  , githubRepository :: Text
  , githubRunId :: Integer
  , githubToken :: Text
  }

getWorkflowEnv :: IO WorkflowEnv
getWorkflowEnv =
  WorkflowEnv
    <$> (pack <$> getEnv "GITHUB_COMMIT_SHA")
    <*> (pack <$> getEnv "GITHUB_REPOSITORY")
    <*> (Unsafe.read <$> getEnv "GITHUB_RUN_ID")
    <*> (pack <$> getEnv "GITHUB_TOKEN")

main :: IO ()
main = do
  Dotenv.loadFile Dotenv.defaultConfig
  env <- getWorkflowEnv

  withTracerProvider $ \tracerProvider -> do
    let
      tracerName = fromString $ "gha-" <> unpack env.githubRepository
      tracer = makeTracer tracerProvider tracerName tracerOptions

    putStrLn "Hello world"

withTracerProvider :: MonadUnliftIO m => (TracerProvider -> m a) -> m a
withTracerProvider =
  bracket
    (liftIO initializeGlobalTracerProvider)
    (liftIO . shutdownTracerProvider)
