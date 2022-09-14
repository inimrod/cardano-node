{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Nodes
  ( addColumnsForConnected
  , addDatasetsForConnected
  , checkNoNodesState
  , updateNodesUI
  , updateNodesUptime
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad (forM_, unless, when)
import           Control.Monad.Extra (whenJust)
import           Data.List (find)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Set (Set, (\\))
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Read (decimal, double)
import           Data.Time.Calendar (diffDays)
import           Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, utctDay)
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Word (Word64)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.State.Logs
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Column
import           Cardano.Tracer.Handlers.RTView.UI.HTML.NoNodes
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Logs
import           Cardano.Tracer.Handlers.RTView.Update.NodeInfo
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

updateNodesUI
  :: TracerEnv
  -> DisplayedElements
  -> ErasSettings
  -> NonEmpty LoggingParams
  -> Colors
  -> DatasetsIndices
  -> Errors
  -> UI.Timer
  -> UI.Timer
  -> LiveViewTimers
  -> LastLiveViewItems
  -> UI ()
updateNodesUI tracerEnv@TracerEnv{teConnectedNodes, teAcceptedMetrics, teSavedTO}
              displayedElements nodesEraSettings loggingConfig colors datasetIndices
              nodesErrors updateErrorsTimer noNodesProgressTimer lvTimers llvItems = do
  (connected, displayedEls) <- liftIO . atomically $ (,)
    <$> readTVar teConnectedNodes
    <*> readTVar displayedElements
  -- Check connected/disconnected nodes since previous UI's update.
  let displayed = S.fromList $ M.keys displayedEls
  when (connected /= displayed) $ do
    let disconnected   = displayed \\ connected -- In 'displayed' but not in 'connected'.
        newlyConnected = connected \\ displayed -- In 'connected' but not in 'displayed'.
    deleteColumnsForDisconnected connected disconnected
    addColumnsForConnected
      tracerEnv
      newlyConnected
      loggingConfig
      nodesErrors
      updateErrorsTimer
      lvTimers
    checkNoNodesState connected noNodesProgressTimer
    askNSetNodeInfo tracerEnv newlyConnected displayedElements
    addDatasetsForConnected tracerEnv newlyConnected colors datasetIndices
    restoreLastHistoryOnCharts tracerEnv datasetIndices newlyConnected
    addLiveViewTimers tracerEnv lvTimers llvItems newlyConnected
    deleteLiveViewTimers lvTimers llvItems disconnected
    liftIO $
      updateDisplayedElements displayedElements connected
  setBlockReplayProgress connected teAcceptedMetrics
  setChunkValidationProgress connected teSavedTO
  setLedgerDBProgress connected teSavedTO
  setProducerMode connected teAcceptedMetrics
  setLeadershipStats connected displayedElements teAcceptedMetrics
  setEraEpochInfo connected displayedElements teAcceptedMetrics nodesEraSettings

addColumnsForConnected
  :: TracerEnv
  -> Set NodeId
  -> NonEmpty LoggingParams
  -> Errors
  -> UI.Timer
  -> LiveViewTimers
  -> UI ()
addColumnsForConnected tracerEnv newlyConnected loggingConfig nodesErrors updateErrorsTimer lvTimers = do
  unless (S.null newlyConnected) $ do
    window <- askWindow
    findAndShow window "main-table-container"
  forM_ newlyConnected $
    addNodeColumn
      tracerEnv
      loggingConfig
      nodesErrors
      updateErrorsTimer
      lvTimers

addDatasetsForConnected
  :: TracerEnv
  -> Set NodeId
  -> Colors
  -> DatasetsIndices
  -> UI ()
addDatasetsForConnected tracerEnv newlyConnected colors datasetIndices = do
  unless (S.null newlyConnected) $ do
    window <- askWindow
    findAndShow window "main-charts-container"
  forM_ newlyConnected $
    addNodeDatasetsToCharts tracerEnv colors datasetIndices

deleteColumnsForDisconnected
  :: Set NodeId
  -> Set NodeId
  -> UI ()
deleteColumnsForDisconnected connected disconnected = do
  window <- askWindow
  forM_ disconnected $ deleteNodeColumn window
  when (S.null connected) $ do
    findAndHide window "main-table-container"
    findAndHide window "main-charts-container"
  -- Please note that we don't remove historical data from charts
  -- for disconnected node. Because the user may want to see the
  -- historical data even for the node that already disconnected.

addLiveViewTimers
  :: TracerEnv
  -> LiveViewTimers
  -> LastLiveViewItems
  -> Set NodeId
  -> UI ()
addLiveViewTimers tracerEnv lvTimers llvItems newlyConnected =
  forM_ newlyConnected $ \nodeId -> do
    timer <- UI.timer # set UI.interval 1000
    on UI.tick timer . const $ updateLogsLiveView tracerEnv llvItems nodeId
    addLiveViewTimer lvTimers nodeId timer

deleteLiveViewTimers
  :: LiveViewTimers
  -> LastLiveViewItems
  -> Set NodeId
  -> UI ()
deleteLiveViewTimers lvTimers llvItems disconnected =
  forM_ disconnected $ \nodeId -> do
    stopLiveViewTimer lvTimers nodeId
    liftIO . atomically $ do
      modifyTVar' lvTimers $ M.delete nodeId
      modifyTVar' llvItems $ M.delete nodeId

checkNoNodesState
  :: Set NodeId
  -> UI.Timer
  -> UI ()
checkNoNodesState connected noNodesProgressTimer = do
  window <- askWindow
  if S.null connected
    then showNoNodes window noNodesProgressTimer
    else hideNoNodes window noNodesProgressTimer

updateNodesUptime
  :: TracerEnv
  -> DisplayedElements
  -> UI ()
updateNodesUptime tracerEnv displayedElements = do
  now <- systemToUTCTime <$> liftIO getSystemTime
  displayed <- liftIO $ readTVarIO displayedElements
  forConnectedUI_ tracerEnv $ getUptimeForNode now displayed
 where
  getUptimeForNode now displayed nodeId@(NodeId anId) = do
    let nodeStartElId   = anId <> "__node-start-time"
        nodeUptimeDElId = anId <> "__node-uptime-days"
        nodeUptimeHElId = anId <> "__node-uptime-hours"
        nodeUptimeMElId = anId <> "__node-uptime-mins"
        nodeUptimeSElId = anId <> "__node-uptime-secs"
    case getDisplayedValuePure displayed nodeId nodeStartElId of
      Nothing -> return ()
      Just tsRaw ->
        case readMaybe (T.unpack tsRaw) of
          Nothing -> return ()
          Just (startTime :: UTCTime) -> do
            let uptimeDiff = now `diffUTCTime` startTime
                uptime     = uptimeDiff `addUTCTime` nullTime
                hoursNum   = formatTime defaultTimeLocale "%H" uptime
                minsNum    = formatTime defaultTimeLocale "%M" uptime
                secsNum    = formatTime defaultTimeLocale "%S" uptime
                daysNum    = utctDay uptime `diffDays` utctDay nullTime
            setTextValues
              [ (nodeUptimeDElId, showT daysNum   <> "d")
              , (nodeUptimeHElId, T.pack hoursNum <> "h")
              , (nodeUptimeMElId, T.pack minsNum  <> "m")
              , (nodeUptimeSElId, T.pack secsNum  <> "s")
              ]

setBlockReplayProgress
  :: Set NodeId
  -> AcceptedMetrics
  -> UI ()
setBlockReplayProgress connected acceptedMetrics = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ connected $ \nodeId ->
    whenJust (M.lookup nodeId allMetrics) $ \(ekgStore, _) -> do
      metrics <- liftIO $ getListOfMetrics ekgStore
      whenJust (lookup "ChainDB.BlockReplayProgress" metrics) $ \metricValue ->
        updateBlockReplayProgress nodeId metricValue
 where
  updateBlockReplayProgress (NodeId anId) mValue =
    case double mValue of
      Left _ -> return ()
      Right (progressPct, _) -> do
        let nodeBlockReplayElId = anId <> "__node-block-replay"
            progressPctS = T.pack $ show progressPct
        if "100" `T.isInfixOf` progressPctS
          then setTextAndClasses nodeBlockReplayElId "100&nbsp;%" "rt-view-percent-done"
          else setTextValue nodeBlockReplayElId $ progressPctS <> "&nbsp;%"

setChunkValidationProgress
  :: Set NodeId
  -> SavedTraceObjects
  -> UI ()
setChunkValidationProgress connected savedTO = do
  savedTraceObjects <- liftIO $ readTVarIO savedTO
  forM_ connected $ \nodeId@(NodeId anId) ->
    whenJust (M.lookup nodeId savedTraceObjects) $ \savedTOForNode -> do
      let nodeChunkValidationElId = anId <> "__node-chunk-validation"
      forM_ (M.toList savedTOForNode) $ \(namespace, (trObValue, _, _)) ->
        case namespace of
          "ChainDB.ImmutableDBEvent.ChunkValidation.ValidatedChunk" ->
            -- In this case we don't need to check if the value differs from displayed one,
            -- because this 'TraceObject' is forwarded only with new values, and after 100%
            -- the node doesn't forward it anymore.
            --
            -- Example: "Validated chunk no. 2262 out of 2423. Progress: 93.36%"
            case T.words trObValue of
              [_, _, _, current, _, _, from, _, progressPct] ->
                setTextValue nodeChunkValidationElId $
                             T.init progressPct <> "&nbsp;%: no. " <> current <> " from " <> T.init from
              _ -> return ()
          "ChainDB.ImmutableDBEvent.ValidatedLastLocation" ->
            setTextAndClasses nodeChunkValidationElId "100&nbsp;%" "rt-view-percent-done"
          _ -> return ()

setLedgerDBProgress
  :: Set NodeId
  -> SavedTraceObjects
  -> UI ()
setLedgerDBProgress connected savedTO = do
  savedTraceObjects <- liftIO $ readTVarIO savedTO
  forM_ connected $ \nodeId@(NodeId anId) ->
    whenJust (M.lookup nodeId savedTraceObjects) $ \savedTOForNode -> do
      let nodeLedgerDBUpdateElId = anId <> "__node-update-ledger-db"
      forM_ (M.toList savedTOForNode) $ \(namespace, (trObValue, _, _)) ->
        case namespace of
          "ChainDB.InitChainSelEvent.UpdateLedgerDb" ->
            -- In this case we don't need to check if the value differs from displayed one,
            -- because this 'TraceObject' is forwarded only with new values, and after 100%
            -- the node doesn't forward it anymore.
            --
            -- Example: "Pushing ledger state for block b1e6...fc5a at slot 54495204. Progress: 3.66%"
            case T.words trObValue of
              [_, _, _, _, _, _, _, _, _, _, progressPct] -> do
                if "100" `T.isInfixOf` progressPct
                  then setTextAndClasses nodeLedgerDBUpdateElId "100&nbsp;%" "rt-view-percent-done"
                  else setTextValue nodeLedgerDBUpdateElId $ T.init progressPct <> "&nbsp;%"
              _ -> return ()
          _ -> return ()

setProducerMode
  :: Set NodeId
  -> AcceptedMetrics
  -> UI ()
setProducerMode connected acceptedMetrics = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ connected $ \nodeId@(NodeId anId) ->
    whenJust (M.lookup nodeId allMetrics) $ \(ekgStore, _) ->
      forMM_ (liftIO $ getListOfMetrics ekgStore) $ \(mName, _) ->
        case mName of
          "Forge.NodeIsLeader"    -> showProducerMode anId
          "Forge.NodeIsLeaderNum" -> showProducerMode anId
          _ -> return ()
 where
  -- The presence of these metrics is a proof that this node is
  -- configured as a producer, so display corresponding icon.
  showProducerMode anId = do
    window <- askWindow
    findAndSet showInline window (anId <> "__node-producer-label")

setLeadershipStats
  :: Set NodeId
  -> DisplayedElements
  -> AcceptedMetrics
  -> UI ()
setLeadershipStats connected displayed acceptedMetrics = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ connected $ \nodeId@(NodeId anId) ->
    whenJust (M.lookup nodeId allMetrics) $ \(ekgStore, _) -> do
      metrics <- liftIO $ getListOfMetrics ekgStore
      forM_ metrics $ \(mName, mValue) ->
        case mName of
          -- How many times this node was a leader.
          "Forge.NodeIsLeaderNum"    -> setDisplayedValue nodeId displayed (anId <> "__node-leadership") mValue
          -- How many blocks were forged by this node.
          "Forge.BlocksForgedNum"    -> setDisplayedValue nodeId displayed (anId <> "__node-forged-blocks") mValue
          -- How many times this node could not forge.
          "Forge.NodeCannotForgeNum" -> setDisplayedValue nodeId displayed (anId <> "__node-cannot-forge") mValue
          -- How many slots were missed in this node.
          "Forge.SlotsMissed"        -> setDisplayedValue nodeId displayed (anId <> "__node-missed-slots") mValue
          _ -> return ()

setEraEpochInfo
  :: Set NodeId
  -> DisplayedElements
  -> AcceptedMetrics
  -> ErasSettings
  -> UI ()
setEraEpochInfo connected displayed acceptedMetrics nodesEraSettings = do
  allSettings <- liftIO $ readTVarIO nodesEraSettings
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ connected $ \nodeId@(NodeId anId) -> do
    epochS <-
      case M.lookup nodeId allMetrics of
        Just (ekgStore, _) -> do
          metrics <- liftIO $ getListOfMetrics ekgStore
          return $ fromMaybe "" $ lookup "ChainDB.Epoch" metrics
        Nothing -> return ""
    unless (T.null epochS) $
      setDisplayedValue nodeId displayed (anId <> "__node-epoch-num") epochS

    whenJust (M.lookup nodeId allSettings) $ \settings -> do
      setDisplayedValue nodeId displayed (anId <> "__node-era") $ esEra settings
      updateEpochInfo settings nodeId epochS
      updateEpochSlotProgress settings nodeId allMetrics
 where
  updateEpochInfo settings (NodeId anId) epochS =
    unless (T.null epochS) $ do
      let epochNum = readInt epochS 0
      whenJust (getEndOfCurrentEpoch settings epochNum) $ \(start, end) -> do
        now <- systemToUTCTime <$> liftIO getSystemTime
        if start < now && now < end
          then do
            -- We're synced and on the current epoch.
            let diffFromNowToEnd = end `diffUTCTime` now
                timeLeft  = diffFromNowToEnd `addUTCTime` nullTime
                daysNum   = utctDay timeLeft `diffDays` utctDay nullTime
                hoursNum   = formatTime defaultTimeLocale "%H" timeLeft
                minsNum    = formatTime defaultTimeLocale "%M" timeLeft
                secsNum    = formatTime defaultTimeLocale "%S" timeLeft
                timeLeftF  =     showT daysNum  <> "d "
                             <> T.pack hoursNum <> "h "
                             <> T.pack minsNum  <> "m "
                             <> T.pack secsNum  <> "s"
            setTextValue (anId <> "__node-epoch-end") timeLeftF
          else do
            -- We're out of date (sync in progress), so just display the end date.
            let endDateF = T.pack $ formatTime defaultTimeLocale "%D %T" end
            setTextValue (anId <> "__node-epoch-end") endDateF

  getEndOfCurrentEpoch EraSettings{esEra, esSlotLengthInS, esEpochLength} currentEpoch =
    case lookup esEra epochsInfo of
      Nothing ->
        -- So, there is no such an era. The possible reason: this is "too new" era
        -- (not officially forked yet). Try to find corresponding epoch directly.
        case find (\(_, (_, firstEpoch)) -> currentEpoch >= firstEpoch) epochsInfo of
          Nothing -> Nothing
          Just (_, (epochStart, firstEpoch)) -> getEpochDates epochStart firstEpoch
      Just (epochStart, firstEpoch) -> getEpochDates epochStart firstEpoch
   where
    getEpochDates startDate firstEpoch =
      let elapsedEpochsInEra = currentEpoch - firstEpoch
          epochLengthInS = esSlotLengthInS * esEpochLength
          secondsFromEpochStartToEpoch = epochLengthInS * elapsedEpochsInEra
          !dateOfEpochStart = startDate + fromIntegral secondsFromEpochStartToEpoch
          !dateOfEpochEnd = dateOfEpochStart + fromIntegral epochLengthInS
      in Just (s2utc dateOfEpochStart, s2utc dateOfEpochEnd)

  updateEpochSlotProgress EraSettings{esEpochLength} nodeId@(NodeId anId) allMetrics =
    whenJust (M.lookup nodeId allMetrics) $ \(ekgStore, _) -> do
      metrics <- liftIO $ getListOfMetrics ekgStore
      whenJust (lookup "ChainDB.SlotInEpoch" metrics) $ \slotInEpochS ->
        case decimal slotInEpochS of
          Left _ -> return ()
          Right (slotInEpoch :: Int, _) -> do
            let slotsPct :: Double
                slotsPct = fromIntegral slotInEpoch / fromIntegral (esEpochLength `div` 100)
                slotsPctF = printf "%.1f" slotsPct <> "%"
            setTextValue (anId <> "__node-epoch-progress-label") (T.pack slotsPctF)
            window <- askWindow
            findAndSet (set style [("width", slotsPctF)]) window (anId <> "__node-epoch-progress")

type EraName         = T.Text
type FirstEpochInEra = Int
type EraStartPOSIX   = Word64

-- It is taken from 'cardano-ledger' wiki topic "First-Block-of-Each-Era".
epochsInfo :: [(EraName, (EraStartPOSIX, FirstEpochInEra))]
epochsInfo =
  [ ("Shelley", (1596073491, 208)) -- 07/30/2020 1:44:51 AM GMT
  , ("Allegra", (1608169491, 236)) -- 12/17/2020 1:44:51 AM GMT
  , ("Mary",    (1614649491, 251)) -- 03/02/2021 1:44:51 AM GMT
  , ("Alonzo",  (1634953491, 298)) -- 10/23/2021 1:44:51 AM GMT, start of new protocol.
  ]
