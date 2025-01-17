{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Cardano.Benchmarking.Compiler
where

import           Prelude

import           Control.Applicative (liftA2)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.CPS

import           Data.Dependent.Sum ( (==>) )
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.Api
import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.NixOptions
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store (Name(..), WalletName)
import           Cardano.Benchmarking.Script.Types

data CompileError where
  SomeCompilerError :: CompileError
  deriving (Show)
type Compiler a = RWST NixServiceOptions (DList Action) Int (Except CompileError) a

maxOutputsPerTx :: Int
maxOutputsPerTx = 30

type SrcWallet = WalletName
type DstWallet = WalletName

compileOptions :: NixServiceOptions -> Either CompileError [Action]
compileOptions opts = runCompiler opts compileToScript

runCompiler ::NixServiceOptions -> Compiler () -> Either CompileError [Action]
runCompiler o c = case runExcept $ runRWST c o 0 of
  Left err -> Left err
  Right ((), _ , l) -> Right $ DL.toList l

testCompiler :: NixServiceOptions -> Compiler a -> Either CompileError (a, Int, [Action])
testCompiler o c = case runExcept $ runRWST c o 0 of
  Left err -> Left err
  Right (a, s , l) -> Right (a, s, DL.toList l)

compileToScript :: Compiler ()
compileToScript = do
  initConstants
  emit . StartProtocol =<< askNixOption getNodeConfigFile
  genesisWallet <- importGenesisFunds
  collateralWallet <- addCollaterals genesisWallet
  splitWallet <- splittingPhase genesisWallet
  benchmarkingPhaseNew splitWallet collateralWallet

initConstants :: Compiler ()
initConstants = do
  setN TTxAdditionalSize     _nix_add_tx_size
  setN TFee                  _nix_tx_fee
  setN TLocalSocket          _nix_localNodeSocketPath
  setConst  TTTL             1000000
  where
    setConst :: Tag v -> v -> Compiler ()
    setConst key val = emit $ Set $ key ==> val 

    setN :: Tag v -> (NixServiceOptions -> v) -> Compiler ()
    setN key s = askNixOption s >>= setConst key

importGenesisFunds :: Compiler WalletName
importGenesisFunds = do
  logMsg "Importing Genesis Fund."
  wallet <- newWallet "genesis_wallet"
  era <- askNixOption _nix_era
  fee <- askNixOption _nix_tx_fee  
  cmd1 (ReadSigningKey $ KeyName "pass-partout") _nix_sigKey
  emit $ Submit era LocalSocket $ SecureGenesis fee wallet (KeyName "pass-partout") (KeyName "pass-partout")
  delay
  logMsg "Importing Genesis Fund. Done."
  return wallet

addCollaterals :: SrcWallet -> Compiler (Maybe WalletName)
addCollaterals src = do
  era <- askNixOption _nix_era
  isAnyPlutusMode >>= \case
    False -> return Nothing
    True -> do
      logMsg "Create collaterals."
      safeCollateral <- _safeCollateral <$> evilFeeMagic
      collateralWallet <- newWallet "collateral_wallet"
      fee <- askNixOption _nix_tx_fee
      let generator = Split fee src
                        (PayToAddr (KeyName "pass-partout") collateralWallet)
                        (PayToAddr (KeyName "pass-partout") src)
                        [ safeCollateral ]
      emit $ Submit era LocalSocket generator
      logMsg "Create collaterals. Done."
      return $ Just collateralWallet

splittingPhase :: SrcWallet -> Compiler DstWallet
splittingPhase srcWallet = do
  (NumberOfTxs tx_count) <- askNixOption _nix_tx_count
  (NumberOfInputsPerTx inputs_per_tx) <- askNixOption _nix_inputs_per_tx
  tx_fee <- askNixOption _nix_tx_fee
  era <- askNixOption _nix_era  
  minValuePerInput <- _minValuePerInput <$> evilFeeMagic
  finalDest <- newWallet "final_split_wallet"
  splitSteps <- splitSequenceWalletNames srcWallet finalDest $ unfoldSplitSequence tx_fee minValuePerInput (tx_count * inputs_per_tx)
  isPlutus <- isAnyPlutusMode
  forM_ (init splitSteps) $ createChange False era
  createChange isPlutus era $ last splitSteps
  return finalDest
 where
  createChange :: Bool -> AnyCardanoEra -> (SrcWallet, DstWallet, Split) -> Compiler ()
  createChange isPlutus era (src, dst, split) = do
    logMsg $ Text.pack $ "Splitting step: " ++ show split
    tx_fee <- askNixOption _nix_tx_fee
    payMode <- if isPlutus then plutusPayMode dst else return $ PayToAddr (KeyName "pass-partout") dst
    let generator = case split of
          SplitWithChange lovelace count -> Split tx_fee src payMode (PayToAddr  (KeyName "pass-partout") src) $ replicate count lovelace
          FullSplits txCount -> Take txCount $ Cycle $ SplitN tx_fee src payMode maxOutputsPerTx
    emit $ Submit era LocalSocket generator
    delay
    logMsg "Splitting step: Done"    

  plutusPayMode :: DstWallet -> Compiler PayMode
  plutusPayMode dst = do
    autoMode <- isPlutusAutoMode
    scriptSpec <- if autoMode
      then ScriptSpec <$> askNixOption _nix_plutusLoopScript <*> pure AutoScript
      else do
        executionUnits <- ExecutionUnits <$> askNixOption _nix_executionMemory <*> askNixOption _nix_executionSteps
        debugMode <- askNixOption _nix_debugMode
        budget <- (if debugMode then CheckScriptBudget else StaticScriptBudget)
                    <$> (ScriptDataNumber <$> askNixOption _nix_plutusData)
                    <*> (ScriptDataNumber <$> askNixOption _nix_plutusRedeemer)
                    <*> pure executionUnits
        ScriptSpec <$> askNixOption _nix_plutusScript <*> pure budget
    return $ PayToScript scriptSpec dst

-- Generate src and dst wallet names for a splitSequence.
-- testCompiler (error "opts") $ splitSequenceWalletNames (WalletName "w1") (WalletName "w2") (unfoldSplitSequence 1 1000 10000)

data Split
  = SplitWithChange Lovelace Int
  | FullSplits Int
  deriving Show

splitSequenceWalletNames :: SrcWallet -> DstWallet -> [Split] -> Compiler [ (SrcWallet, DstWallet, Split) ]
splitSequenceWalletNames _src _dst [] = return []
splitSequenceWalletNames src dst [ split ] = return [( src, dst, split )]
splitSequenceWalletNames src dst (split: rest) = do
  tempWallet <- newWallet "change_wallet"
  l <- splitSequenceWalletNames tempWallet dst rest
  return $ ( src, tempWallet, split) : l

unfoldSplitSequence :: Lovelace -> Lovelace -> Int -> [ Split ]
unfoldSplitSequence fee value outputs
  = if outputs < maxOutputsPerTx
      then [ SplitWithChange value outputs ]
      else
        let txs = outputs `divCeiling` maxOutputsPerTx
        in unfoldSplitSequence fee (value * fromIntegral maxOutputsPerTx + fee) txs ++ [ FullSplits txs ]
  where
    -- maximal number of outputs in a TX.
    -- todo: this must be in sync with Scipt/Core.hs
    divCeiling a b = case divMod a b of
     (x, 0) -> x
     (x, _rest) -> x+1

benchmarkingPhaseNew :: WalletName -> Maybe WalletName -> Compiler ()
benchmarkingPhaseNew wallet collateralWallet = do
  debugMode <- askNixOption _nix_debugMode
  targetNodes <- askNixOption _nix_targetNodes
  extraArgs <- evilValueMagic
  tps <- askNixOption _nix_tps
  era <- askNixOption _nix_era
  (NumberOfTxs txCount) <- askNixOption _nix_tx_count
  let
    submitMode = if debugMode
        then LocalSocket
        else Benchmark targetNodes (ThreadName "tx-submit-benchmark") tps  extraArgs
    generator = Take txCount $ Cycle $ BechmarkTx wallet extraArgs collateralWallet
  emit $ Submit era submitMode generator
  unless debugMode $ do
    emit $ WaitBenchmark $ ThreadName "tx-submit-benchmark"

data Fees = Fees {
    _safeCollateral :: Lovelace
  , _minValuePerInput :: Lovelace
  }
  
evilFeeMagic :: Compiler Fees
evilFeeMagic = do
  (Quantity tx_fee) <- lovelaceToQuantity <$> askNixOption _nix_tx_fee
  plutusMode <- askNixOption _nix_plutusMode  
  (NumberOfInputsPerTx inputs_per_tx) <- askNixOption _nix_inputs_per_tx
  (NumberOfOutputsPerTx outputs_per_tx) <- askNixOption _nix_outputs_per_tx  
  (Quantity min_utxo_value)  <- lovelaceToQuantity <$> askNixOption _nix_min_utxo_value
  let
    scriptFees = 5000000;
    collateralPercentage = 200;

    totalFee = if plutusMode
               then tx_fee + scriptFees * fromIntegral inputs_per_tx
               else tx_fee;
    safeCollateral = max ((scriptFees + tx_fee) * collateralPercentage `div` 100) min_utxo_value;
    minTotalValue = min_utxo_value * fromIntegral outputs_per_tx + totalFee;
    minValuePerInput = minTotalValue `div` fromIntegral inputs_per_tx + 1;
  return $ Fees {
      _safeCollateral = fromIntegral safeCollateral
    , _minValuePerInput = fromIntegral minValuePerInput
    }

emit :: Action -> Compiler ()
emit = tell . DL.singleton

logMsg :: Text -> Compiler ()
logMsg = emit . LogMsg

cmd1 :: (v -> Action) -> (NixServiceOptions -> v) -> Compiler ()
cmd1 cmd arg = emit . cmd =<< askNixOption arg
  
askNixOption :: (NixServiceOptions -> v) -> Compiler v
askNixOption = asks

delay :: Compiler ()
delay = cmd1 Delay _nix_init_cooldown

isPlutusMode :: Compiler Bool
isPlutusMode = askNixOption _nix_plutusMode

isPlutusAutoMode :: Compiler Bool
isPlutusAutoMode = askNixOption _nix_plutusAutoMode

isAnyPlutusMode :: Compiler Bool
isAnyPlutusMode = liftA2 (||) isPlutusMode isPlutusAutoMode

newIdentifier :: String -> Compiler String
newIdentifier prefix = do
  n <- get
  put $ succ n
  return $ prefix ++ "_" ++ show n

newWallet :: String -> Compiler WalletName
newWallet n = do
  name <- WalletName <$> newIdentifier n
  emit $ InitWallet name
  return name

-- Approximate the ada values for inputs of the benchmarking Phase
evilValueMagic :: Compiler RunBenchmarkAux
evilValueMagic = do
  (NumberOfInputsPerTx inputsPerTx) <- askNixOption _nix_inputs_per_tx
  (NumberOfOutputsPerTx outputsPerTx) <- askNixOption _nix_outputs_per_tx
  (NumberOfTxs txCount) <- askNixOption _nix_tx_count
  fee <- askNixOption _nix_tx_fee
  minValuePerUTxO <- askNixOption _nix_min_utxo_value
  let
    (Quantity minValue) = lovelaceToQuantity $ fromIntegral outputsPerTx * minValuePerUTxO + fee

  -- this is not totally correct:
  -- beware of rounding errors !
    minValuePerInput = quantityToLovelace $ fromIntegral (if m==0 then d else d+1)
      where
        (d, m) = minValue `divMod` fromIntegral inputsPerTx
  return $  RunBenchmarkAux {
      auxTxCount = txCount
    , auxFee = fee
    , auxOutputsPerTx = outputsPerTx
    , auxInputsPerTx = inputsPerTx
    , auxInputs = inputsPerTx * txCount
    , auxOutputs = inputsPerTx * txCount
    , auxMinValuePerUTxO = minValuePerInput
    }

