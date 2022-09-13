{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.TraceObjects
  ( updateUIBySavedTOs
  ) where

--import           Control.Concurrent.STM.TVar (readTVarIO)
--import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.Utils

updateUIBySavedTOs
  :: TracerEnv
  -> DisplayedElements
  -> UI ()
updateUIBySavedTOs tracerEnv@TracerEnv{teSavedTO2} _ =
  forConnectedUI_ tracerEnv $ \nodeId -> do
    _itemsFromThisNode <- liftIO $ getTraceObjects teSavedTO2 nodeId
    return ()
