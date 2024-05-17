module Freckle.TraceWorkflow.OpenTelemetry
  ( Tracer
  , withTracer
  , SpanData (..)
  , recordSpan
  , recordSpan_
  ) where

import Relude

import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Freckle.TraceWorkflow.GitHub
  ( HasName (..)
  , HasStatus (..)
  , HasTimestamps (..)
  )
import OpenTelemetry.Common (Timestamp (..))
import OpenTelemetry.Trace hiding (inSpan)
import System.Clock (fromNanoSecs)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket)

withTracer :: MonadUnliftIO m => (Tracer -> m a) -> m a
withTracer f =
  bracket
    (liftIO initializeGlobalTracerProvider)
    (liftIO . shutdownTracerProvider)
    $ \tracerProvider -> do
      f $ makeTracer tracerProvider "freckle_trace_workflow_action" tracerOptions

data SpanData = SpanData
  { name :: Text
  , start :: UTCTime
  , end :: UTCTime
  , error :: Maybe Text
  }

toSpanData :: (HasName a, HasTimestamps a, HasStatus a) => a -> SpanData
toSpanData x =
  SpanData
    { name = name x
    , start = startedAt x
    , end = completedAt x
    , error = case status x of
        "failed" -> Just "Failed"
        _ -> Nothing
    }

recordSpan
  :: (MonadUnliftIO m, HasName a, HasTimestamps a, HasStatus a, HasCallStack)
  => Tracer
  -> a
  -> m b
  -- ^ An action to take while span is acting as parent
  -> m ()
recordSpan tracer x f = do
  inSpan'' tracer sd.name args $ \s -> do
    void f
    traverse_ (setStatus s . Error) sd.error
    endSpan s $ Just $ utcTimeToTimestamp $ sd.end
 where
  sd = toSpanData x
  args = defaultSpanArguments {startTime = Just $ utcTimeToTimestamp sd.start}

recordSpan_
  :: (MonadUnliftIO m, HasName a, HasTimestamps a, HasStatus a, HasCallStack)
  => Tracer
  -> a
  -> m ()
recordSpan_ tracer x = recordSpan tracer x $ pure ()

utcTimeToTimestamp :: UTCTime -> Timestamp
utcTimeToTimestamp = Timestamp . fromNanoSecs . utcTimeToNanoSecs

utcTimeToNanoSecs :: UTCTime -> Integer
utcTimeToNanoSecs =
  floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
