{-# LANGUAGE NamedFieldPuns #-}

module Freckle.TraceWorkflow.GitHub
  ( RunWithJobs (..)
  , getRunWithJobs
  , Run (..)
  , getRun
  , Job (..)
  , Step (..)
  , getJobs
  , HasName (..)
  , HasTimestamps (..)
  , HasStatus (..)
  ) where

import Relude

import Data.Aeson
import Data.List.NonEmpty.Extra qualified as NE
import Data.Text (unpack)
import Data.Time (UTCTime)
import Data.Traversable (for)
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization, hUserAgent)

data RunWithJobs = RunWithJobs
  { run :: Run
  , jobs :: NonEmpty Job
  }

getRunWithJobs
  :: MonadIO m => Text -> Text -> Text -> Int -> m (Maybe RunWithJobs)
getRunWithJobs token owner repo runId = do
  mJobs <- NE.nonEmpty <$> getJobs token owner repo runId

  for mJobs $ \jobs -> do
    run <- getRun token owner repo runId
    pure $ RunWithJobs {run, jobs}

data Run = Run
  { name :: Text
  , status :: Text
  , conclusion :: Maybe Text
  , run_attempt :: Int
  , run_started_at :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

getRun :: MonadIO m => Text -> Text -> Text -> Int -> m Run
getRun token owner repo runId = do
  github token
    $ "/repos/"
    <> unpack owner
    <> "/"
    <> unpack repo
    <> "/actions/runs/"
    <> show runId

data Job = Job
  { name :: Text
  , status :: Text
  , conclusion :: Maybe Text
  , started_at :: UTCTime
  , completed_at :: UTCTime
  , steps :: [Step]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Step = Step
  { name :: Text
  , status :: Text
  , conclusion :: Maybe Text
  , started_at :: UTCTime
  , completed_at :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data JobsPage = JobsPage
  { total_count :: Int
  , jobs :: [Job]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

getJobs :: MonadIO m => Text -> Text -> Text -> Int -> m [Job]
getJobs token owner repo runId = do
  page <-
    github @_ @JobsPage token
      $ "/repos/"
      <> unpack owner
      <> "/"
      <> unpack repo
      <> "/actions/runs/"
      <> show runId
      <> "/jobs"
      <> "?per_page=100"

  -- TODO: we don't expect or support this, but we can check and error at least
  -- when (page.total_count > 100)

  pure page.jobs

github :: (MonadIO m, FromJSON a) => Text -> String -> m a
github token path = do
  req <- liftIO $ parseRequest $ "https://api.github.com" <> path
  fmap getResponseBody
    $ httpJSON
    $ addRequestHeader hAuthorization ("token " <> encodeUtf8 token)
    $ addRequestHeader hUserAgent "freckle-trace-workflow-action"
    $ addRequestHeader "X-GitHub-Api-Version" "2022-11-28" req

class HasName a where
  name :: a -> Text

instance HasName RunWithJobs where
  name = name . (.run)

instance HasName Run where
  name run = run.name

instance HasName Job where
  name job = job.name

instance HasName Step where
  name step = step.name

instance HasTimestamps RunWithJobs where
  startedAt x = x.run.run_started_at
  completedAt x = NE.maximum1 $ (.completed_at) <$> x.jobs

class HasTimestamps a where
  startedAt :: a -> UTCTime
  completedAt :: a -> UTCTime

instance HasTimestamps Job where
  startedAt job = job.started_at
  completedAt job = job.completed_at

instance HasTimestamps Step where
  startedAt step = step.started_at
  completedAt step = step.completed_at

class HasStatus a where
  status :: a -> Text

instance HasStatus RunWithJobs where
  status x = x.run.status

instance HasStatus Run where
  status run = run.status

instance HasStatus Job where
  status job = job.status

instance HasStatus Step where
  status step = step.status
