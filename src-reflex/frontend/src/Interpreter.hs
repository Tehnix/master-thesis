{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Control.Monad.Freer
import Control.Monad.IO.Class (liftIO)

import Common.Types
import Common.Computation
import JavaScriptFFI (logToiOS, setiOSLabel1, setiOSLabel2)
import Text.Read (readMaybe)
import Offloader (offload)


runProgramM :: Eff '[Operation, Computation, IO] a -> IO a
runProgramM p = runM . runComputationM . runEventM $ p

runEventM :: forall effs a. LastMember IO effs => Eff (Operation ': effs) a -> Eff effs a
runEventM = interpretM $ \case
  WriteOutput s -> logToiOS (show s)

runComputationM :: forall effs a. LastMember IO effs => Eff (Computation ': effs) a -> Eff effs a
runComputationM = interpretM $ \case
  p@(IsPrime i) -> do
    mComputation <- offload (serialize p) (serialize i)
    let computation = case mComputation of
          Nothing -> computeIsPrime i
          Just mSC ->  maybe False id ((readMaybe (read mSC :: String)) :: Maybe Bool)
    liftIO . logToiOS $ "Computation is: " ++ show computation
    setiOSLabel1 $ show computation
    pure computation
  p@(FactorialLength i) -> do
    mComputation <- offload (serialize p) (serialize i)
    let computation = case mComputation of
          Nothing -> computeFactorialLength i
          Just mSC ->  maybe (-1) id ((readMaybe (read mSC :: String)) :: Maybe Int)
    liftIO . logToiOS $ "Computation is: " ++ show computation
    setiOSLabel2 $ show computation
    pure computation
