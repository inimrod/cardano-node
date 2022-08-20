{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Logs
  ( updateLogsLiveView
  , deleteAllErrorMessages
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM, forM_, void)
import           Control.Monad.Extra (whenJust, whenJustM, whenM)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Logging (SeverityS (..))

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types

-- | Update log items in a corresponding modal window.
updateLogsLiveView
  :: TracerEnv
  -> UI ()
updateLogsLiveView tracerEnv@TracerEnv{teSavedTO} = do
  window <- askWindow
  savedTO <- liftIO $ readTVarIO teSavedTO
  forConnectedUI_ tracerEnv $ \nodeId@(NodeId anId) ->
    whenJust (M.lookup nodeId savedTO) $ \savedTOForNode ->
      forM_ (M.toList savedTOForNode) $ \(_, _trObInfo@(_, severity, _)) ->
        whenM (traceObjectShouldBeAdded severity) $
          whenJustM (UI.getElementById window (T.unpack anId <> "__node-logs-live-view-tbody")) $ \el -> do
            let onlyNewErrors = []
            doAddErrorRows nodeId onlyNewErrors el

traceObjectShouldBeAdded :: SeverityS -> UI Bool
traceObjectShouldBeAdded _ = return True

doAddErrorRows
  :: NodeId
  -> [ErrorInfo]
  -> Element
  -> UI ()
doAddErrorRows nodeId errorsToAdd parentEl = do
  errorRows <-
    forM errorsToAdd $ \(_errorIx, (msg, sev, ts)) ->
      mkErrorRow nodeId msg sev ts
  void $ element parentEl #+ errorRows
 where
  mkErrorRow (NodeId anId) msg sev ts = do
    copyErrorIcon <- image "has-tooltip-multiline has-tooltip-left rt-view-copy-icon" copySVG
                           # set dataTooltip "Click to copy this error"
    on UI.click copyErrorIcon . const $
      copyTextToClipboard $ errorToCopy ts sev msg

    return $
      UI.tr #. (T.unpack anId <> "-node-error-row") #+
        [ UI.td #+
            [ UI.span # set text (preparedTS ts)
            ]
        , UI.td #+
            [ UI.span #. "tag is-medium is-danger" # set text (show sev)
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
            [ element copyErrorIcon
            ]
        ]

  preparedTS = formatTime defaultTimeLocale "%b %e, %Y %T"

  errorToCopy ts sev msg = "[" <> preparedTS ts <> "] [" <> show sev <> "] [" <> T.unpack msg <> "]"

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
