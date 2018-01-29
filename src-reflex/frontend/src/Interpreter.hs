{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Control.Monad.Freer

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
          Just c -> maybe False id ((readMaybe c) :: Maybe Bool)
    setiOSLabel1 $ show computation
    pure computation
  p@(FactorialLength i) -> do
    mComputation <- offload (serialize p) (serialize i)
    let computation = case mComputation of
          Nothing -> computeFactorialLength i
          Just c -> maybe 0 id ((readMaybe c) :: Maybe Int)
    setiOSLabel2 $ show computation
    pure computation
