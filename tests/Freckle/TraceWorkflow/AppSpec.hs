-- |
--
-- This test expects an otel-collector to be running with the file exporter
-- configured to write to @./file-exporter/trace.json@. It traces a workflow
-- fixture and then asserts the data in @trace.json@ is correct.
module Freckle.TraceWorkflow.AppSpec
  ( spec
  ) where

import Relude hiding (trace)

import Control.Error.Util (note)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.List.NonEmpty qualified as NE
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Format.ISO8601
import Freckle.TraceWorkflow.App (traceRunWithJobs)
import Freckle.TraceWorkflow.GitHub
  ( Job (..)
  , Run (..)
  , RunWithJobs (..)
  , Step (..)
  )
import Test.Hspec
import UnliftIO.Exception (throwString)

spec :: Spec
spec = do
  describe "traceRunWithJobs" $ do
    it "traces a Run and its Jobs and Steps" $ do
      let
        startTime :: UTCTime
        startTime =
          fromMaybe (error "Did not parse as ISO8601")
            $ iso8601ParseM "2024-05-01T14:05:36Z"

      traceRunWithJobs
        $ RunWithJobs
          { run =
              Run
                { name = "Backend"
                , status = "succeeded"
                , conclusion = Just "complete"
                , run_attempt = 1
                , run_started_at = startTime
                }
          , jobs =
              NE.fromList
                [ Job
                    { name = "changes"
                    , status = "succeeded"
                    , conclusion = Just "succeeded"
                    , started_at = addUTCTime 1 startTime
                    , completed_at = addUTCTime 2 startTime
                    , steps =
                        [ Step
                            { name = "Run dorny/paths-filter"
                            , status = "succeeded"
                            , conclusion = Just "succeeded"
                            , started_at = addUTCTime 1 startTime
                            , completed_at = addUTCTime 2 startTime
                            }
                        ]
                    }
                ]
          }

      trace <- readMostRecentTrace
      trace.resourceSpans `shouldSatisfy` (not . null)

newtype Trace = Trace
  { resourceSpans :: [ResourceSpan]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data ResourceSpan = ResourceSpan
  { resource :: Resource
  , scopeSpans :: [ScopeSpan]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype Resource = Resource
  { attributes :: [Attribute]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data Attribute = Attribute
  { key :: Text
  , value :: AttributeValue
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data AttributeValue = AttributeValue
  { stringValue :: Maybe Text
  , intValue :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data ScopeSpan = ScopeSpan
  { scope :: Scope
  , spans :: [Span]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype Scope = Scope
  { name :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data Span = Span
  { traceId :: Text
  , spanId :: Text
  , parentSpanId :: Text
  , name :: Text
  , kind :: Int
  , startTimeUnixNano :: Text
  , endTimeUnixNano :: Text
  , attributes :: [Attribute]
  , droppedAttributesCount :: Int
  , droppedLinksCount :: Int
  -- , status :: ?
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

readMostRecentTrace :: IO Trace
readMostRecentTrace = do
  mjson <- NE.nonEmpty . BSL8.lines <$> readFileLBS "file-exporter/trace.json"
  either throwString pure $ do
    jsons <- note "No trace written to trace.json" mjson
    eitherDecode @Trace $ NE.last jsons
