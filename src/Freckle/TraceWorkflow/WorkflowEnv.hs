module Freckle.TraceWorkflow.WorkflowEnv
  ( WorkflowEnv (..)
  , getWorkflowEnv
  ) where

import Relude

import Data.Text (pack)
import System.Environment (getEnv)
import Prelude qualified as Unsafe (read)

data WorkflowEnv = WorkflowEnv
  { githubCommitSha :: Text
  , githubOwner :: Text
  , githubRepo :: Text
  , githubRunId :: Int
  , githubToken :: Text
  }

getWorkflowEnv :: IO WorkflowEnv
getWorkflowEnv =
  WorkflowEnv
    <$> (pack <$> getEnv "GITHUB_COMMIT_SHA")
    <*> (pack <$> getEnv "GITHUB_OWNER")
    <*> (pack <$> getEnv "GITHUB_REPO")
    <*> (Unsafe.read <$> getEnv "GITHUB_RUN_ID")
    <*> (pack <$> getEnv "GITHUB_TOKEN")
