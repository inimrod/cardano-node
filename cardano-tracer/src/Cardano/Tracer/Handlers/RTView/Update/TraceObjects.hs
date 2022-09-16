{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.TraceObjects
  ( updateUIBySavedTOs
  ) where

--import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_, void)
import           Control.Monad.Extra (whenJustM)
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

updateUIBySavedTOs
  :: TracerEnv
  -> DisplayedElements
  -> UI ()
updateUIBySavedTOs tracerEnv@TracerEnv{teSavedTO2} _ = do
  window <- askWindow
  whenJustM (UI.getElementById window "node-logs-live-view-tbody") $ \el ->
    forConnectedUI_ tracerEnv $ \nodeId -> do
      nodeName        <- liftIO $ askNodeName tracerEnv nodeId
      nodeColor       <- liftIO $ getSavedColorForNode nodeName
      tosFromThisNode <- liftIO $ getTraceObjects teSavedTO2 nodeId
      forM_ tosFromThisNode $
        doAddItemRow nodeId nodeName nodeColor el

doAddItemRow
  :: NodeId
  -> NodeName
  -> Maybe Color
  -> Element
  -> (Namespace, TraceObjectInfo)
  -> UI ()
doAddItemRow (NodeId anId) nodeName nodeColor parentEl (ns, (msg, sev, ts)) = do
  aRow <- mkItemRow
  void $ element parentEl #+ [aRow]
 where
  mkItemRow = do
    copyItemIcon <- image "has-tooltip-multiline has-tooltip-left rt-view-copy-icon" copySVG
                          # set dataTooltip "Click to copy this error"
    on UI.click copyItemIcon . const $ copyTextToClipboard $
      "[" <> preparedTS ts <> "] [" <> show sev <> "] [" <> T.unpack ns <> "] [" <> T.unpack msg <> "]"

    nodeNameLabel <-
      case nodeColor of
        Nothing -> UI.span # set text (T.unpack nodeName)
        Just (Color code) -> UI.span # set style [("color", code)]
                                     # set text (T.unpack nodeName)

    return $
      UI.tr #. (T.unpack anId <> "-node-logs-live-view-row") #+
        [ UI.td #+
            [ element nodeNameLabel
            ]
        , UI.td #+
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
