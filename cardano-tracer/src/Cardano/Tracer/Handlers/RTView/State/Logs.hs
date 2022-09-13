module Cardano.Tracer.Handlers.RTView.State.Logs
  ( LastLiveViewItems
  , LiveViewTimers
  , addLiveViewTimer
  , getLastLiveViewItem
  , initLastLiveViewItems
  , initLiveViewTimers
  , saveLastLiveViewItem
  , startLiveViewTimer
  , stopLiveViewTimer
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Control.Monad.Extra (whenJustM)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Types (NodeId)

-- | If two 'TraceObject's has the same timestamp and message -
--   we treat them as identical ones.
type TraceObjectMark = (UTCTime, Text)
-- | We have to keep the info about the last 'TraceObject' to prevent
--   adding the same 'TraceObject' in "Live view" window more than once.
type LastLiveViewItems = TVar (Map NodeId TraceObjectMark)

initLastLiveViewItems :: IO LastLiveViewItems
initLastLiveViewItems = newTVarIO M.empty

saveLastLiveViewItem
  :: LastLiveViewItems
  -> NodeId
  -> TraceObjectMark
  -> IO ()
saveLastLiveViewItem llvItems nodeId trObMark = atomically $
  modifyTVar' llvItems $ \currentItems ->
    case M.lookup nodeId currentItems of
      Nothing -> M.insert nodeId trObMark currentItems
      Just _  -> M.adjust (const trObMark) nodeId currentItems

getLastLiveViewItem
  :: LastLiveViewItems
  -> NodeId
  -> IO (Maybe TraceObjectMark)
getLastLiveViewItem llvItems nodeId =
  M.lookup nodeId <$> readTVarIO llvItems

-- | Timers for live view windows: only when the user opened it,
--   the items will be shown (and updated in a real-time).
type LiveViewTimers = TVar (Map NodeId UI.Timer)

initLiveViewTimers :: IO LiveViewTimers
initLiveViewTimers = newTVarIO M.empty

addLiveViewTimer
  :: LiveViewTimers
  -> NodeId
  -> UI.Timer
  -> UI ()
addLiveViewTimer lvTimers nodeId timer = liftIO . atomically $
  modifyTVar' lvTimers $ \currentTimers ->
    case M.lookup nodeId currentTimers of
      Nothing -> M.insert nodeId timer currentTimers
      Just _  -> M.adjust (const timer) nodeId currentTimers

startLiveViewTimer, stopLiveViewTimer
  :: LiveViewTimers
  -> NodeId
  -> UI ()
startLiveViewTimer = triggerLiveViewTimer UI.start
stopLiveViewTimer  = triggerLiveViewTimer UI.stop

triggerLiveViewTimer
  :: (UI.Timer -> UI ())
  -> LiveViewTimers
  -> NodeId
  -> UI ()
triggerLiveViewTimer trigger lvTimers nodeId =
  whenJustM (M.lookup nodeId <$> liftIO (readTVarIO lvTimers)) trigger
