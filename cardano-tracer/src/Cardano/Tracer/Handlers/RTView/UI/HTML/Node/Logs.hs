{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Logs
  ( mkLogsLiveView
  ) where

import           Data.Text (unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

mkLogsLiveView
  :: String
  -> NodeName
  -> UI Element
mkLogsLiveView id' nodeName = do
  _window <- askWindow
  closeIt <- UI.button #. "delete"

  logsLiveViewTable <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card rt-view-logs-live-view-modal" #+
          [ UI.header #. "modal-card-head rt-view-logs-live-view-head" #+
              [ UI.p #. "modal-card-title rt-view-logs-live-view-title" #+
                  [ string "Log items from "
                  , UI.span ## (id' <> "__node-name-for-logs-live-view")
                            #. "has-text-weight-bold"
                            # set text (unpack nodeName)
                  ]
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-logs-live-view-body" #+
              [ UI.div ## (id' <> "__logs-live-view-table-container") #. "table-container" #+
                  [ UI.table ## (id' <> "__logs-live-view-table") #. "table is-fullwidth rt-view-logs-live-view-table" #+
                      [ UI.mkElement "thead" #+
                          [ UI.tr #+
                              [ UI.th #. "rt-view-logs-live-view-timestamp" #+
                                  [ string "Timestamp"
                                  ]
                              , UI.th #. "rt-view-logs-live-view-severity" #+
                                  [ string "Severity"
                                  ]
                              , UI.th #. "rt-view-logs-live-view-namespace" #+
                                  [ string "Namespace"
                                  ]
                              , UI.th #+
                                  [ string "Message"
                                  ]
                              ]
                          ]
                      , UI.mkElement "tbody" ## (id' <> "__node-logs-live-view-tbody")
                                             # set dataState "0"
                                             #+ []
                      ]
                  ]
              ]
          , UI.mkElement "footer" #. "modal-card-foot rt-view-logs-live-view-foot" #+
              [ 
              ]
          ]
      ]
  on UI.click closeIt . const $ element logsLiveViewTable #. "modal"
  return logsLiveViewTable
