{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Logs
  ( updateLogsLiveView
  , updateLogsLiveView'
  , deleteAllErrorMessages
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_, void, when)
import           Control.Monad.Extra (whenJust, whenJustM, whenM)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Logging (SeverityS (..))

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Logs
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

updateLogsLiveView
  :: TracerEnv
  -> LastLiveViewItems
  -> NodeId
  -> UI ()
updateLogsLiveView _ _ _ = return ()

-- | Update log items in a corresponding modal window.
updateLogsLiveView'
  :: TracerEnv
  -> LastLiveViewItems
  -> NodeId
  -> UI ()
updateLogsLiveView' TracerEnv{teSavedTO} llvItems nodeId@(NodeId anId) = do
  savedTO <- liftIO $ readTVarIO teSavedTO
  whenJust (M.lookup nodeId savedTO) $ \savedTOForNode ->
    forM_ (M.toList savedTOForNode) $ \(ns, trObInfo@(msg, severity, ts)) ->
      whenM (traceObjectShouldBeAdded severity) $ do
        let trObInfo' = (ns, trObInfo)
            trObMark = (ts, msg)
        liftIO (getLastLiveViewItem llvItems nodeId) >>= \case
          Nothing -> doAddItem trObInfo' trObMark
          Just (lastTS, lastMsg) ->
            -- We should add this 'TraceObject' only if it wasn't added before.
            if | ts < lastTS -> return () -- Item from the past, ignore it.
               | ts > lastTS -> doAddItem trObInfo' trObMark
               | otherwise   -> when (msg /= lastMsg) $ doAddItem trObInfo' trObMark
 where
  doAddItem trObInfo' trObMark = do
    window <- askWindow
    whenJustM (UI.getElementById window (T.unpack anId <> "__node-logs-live-view-tbody")) $ \el -> do
      doAddItemRow nodeId trObInfo' el
      liftIO $ saveLastLiveViewItem llvItems nodeId trObMark

-- | Check severity filters to lnow if we should add this 'TraceObject' for now.
traceObjectShouldBeAdded :: SeverityS -> UI Bool
traceObjectShouldBeAdded _ = return True

doAddItemRow
  :: NodeId
  -> (Namespace, TraceObjectInfo)
  -> Element
  -> UI ()
doAddItemRow (NodeId anId) (ns, (msg, sev, ts)) parentEl = do
  aRow <- mkItemRow
  void $ element parentEl #+ [aRow]
 where
  mkItemRow = do
    copyItemIcon <- image "has-tooltip-multiline has-tooltip-left rt-view-copy-icon" copySVG
                          # set dataTooltip "Click to copy this error"
    on UI.click copyItemIcon . const $ copyTextToClipboard $
      "[" <> preparedTS ts <> "] [" <> show sev <> "] [" <> T.unpack ns <> "] [" <> T.unpack msg <> "]"

    return $
      UI.tr #. (T.unpack anId <> "-node-logs-live-view-row") #+
        [ UI.td #+
            [ UI.span # set text (preparedTS ts)
            ]
        , UI.td #+
            [ UI.span #. "tag is-medium is-info" # set text (show sev)
            ]
        , UI.td #+
            [ UI.p #. "control" #+
                [ UI.input #. "input rt-view-error-msg-input"
                           # set UI.type_ "text"
                           # set (UI.attr "readonly") "readonly"
                           # set UI.value (T.unpack ns)
                ]
            ]
        , UI.td #+
            [ UI.p #. "control" #+
                [ UI.input #. "input rt-view-error-msg-input"
                           # set UI.type_ "text"
                           # set (UI.attr "readonly") "readonly"
                           # set UI.value (T.unpack msg)
                ]
            ]
        , UI.td #+
            [ element copyItemIcon
            ]
        ]

  preparedTS = formatTime defaultTimeLocale "%b %e, %Y %T"

deleteAllErrorMessages
  :: UI.Window
  -> NodeId
  -> UI ()
deleteAllErrorMessages window _nodeId@(NodeId anId) = do
  -- Delete errors from window.
  findByClassAndDo window (anId <> "-node-logs-live-view-row") delete'
  -- Reset number of currently displayed errors rows.
  whenJustM (UI.getElementById window (T.unpack anId <> "__node-errors-tbody")) $ \el ->
    void $ element el # set dataState "0"
