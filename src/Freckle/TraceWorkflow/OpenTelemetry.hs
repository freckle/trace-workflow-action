module Freckle.TraceWorkflow.OpenTelemetry
  ( Tracer
  , withTracer
  , inSpan
  ) where

import Relude

import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
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

inSpan
  :: (MonadUnliftIO m, HasCallStack)
  => Tracer
  -> Text
  -- ^ Name
  -> UTCTime
  -- ^ Started at
  -> Maybe Text
  -- ^ Error
  -> m UTCTime
  -- ^ Returns ended at
  -> m ()
inSpan tracer name start mError f = inSpan'' tracer name args $ \s -> do
  end <- f
  traverse_ (setStatus s . Error) mError
  endSpan s $ Just $ utcTimeToTimestamp end
 where
  args = defaultSpanArguments {startTime = Just $ utcTimeToTimestamp start}

utcTimeToTimestamp :: UTCTime -> Timestamp
utcTimeToTimestamp = Timestamp . fromNanoSecs . utcTimeToNanoSecs

utcTimeToNanoSecs :: UTCTime -> Integer
utcTimeToNanoSecs =
  floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
