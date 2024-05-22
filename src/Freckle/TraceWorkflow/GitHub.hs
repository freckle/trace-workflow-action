module Freckle.TraceWorkflow.GitHub
  ( Run (..)
  , getRun
  , Job (..)
  , Step (..)
  , getJobs
  -- Timestamps
  , HasTimestamps (..)
  , wasSkipped
  ) where

import           Relude

import           Data.Aeson
import           Data.Text                 (unpack)
import           Data.Time                 (UTCTime)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header (hAuthorization, hUserAgent)

data Run = Run
  { name           :: Text
  , status         :: Text
  , conclusion     :: Maybe Text
  , run_attempt    :: Int
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
  { name         :: Text
  , status       :: Text
  , conclusion   :: Maybe Text
  , started_at   :: UTCTime
  , completed_at :: UTCTime
  , steps        :: [Step]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Step = Step
  { name         :: Text
  , status       :: Text
  , conclusion   :: Maybe Text
  , started_at   :: UTCTime
  , completed_at :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data JobsPage = JobsPage
  { total_count :: Int
  , jobs        :: [Job]
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

class HasTimestamps a where
  startedAt :: a -> UTCTime
  completedAt :: a -> UTCTime

wasSkipped :: HasTimestamps a => a -> Bool
wasSkipped = (==) <$> completedAt <*> startedAt

instance HasTimestamps Job where
  startedAt job = job.started_at
  completedAt job = job.completed_at

instance HasTimestamps Step where
  startedAt step = step.started_at
  completedAt step = step.completed_at
