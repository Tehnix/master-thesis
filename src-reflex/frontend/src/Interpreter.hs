{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter where

import Control.Monad.Freer
import Control.Monad.IO.Class (liftIO)

import Common.Types
import Common.Computation
import JavaScriptFFI (logToiOS, setiOSLabel1, setiOSLabel2, setiOSIncLocalCounter)
import Offloader (offloadComputation)
import Data.Proxy


runProgramM :: Eff '[Operation, Computation, IO] a -> IO a
runProgramM p = runM . runComputationM . runEventM $ p

runLocalProgramM :: Eff '[Operation, Computation, IO] a -> IO a
runLocalProgramM p = runM . runLocalComputationM . runEventM $ p

runEventM :: forall effs a. LastMember IO effs => Eff (Operation ': effs) a -> Eff effs a
runEventM = interpretM $ \case
  WriteOutput _ -> pure ()
  -- WriteOutput s -> logToiOS (show s)

runComputationM :: forall effs a. LastMember IO effs => Eff (Computation ': effs) a -> Eff effs a
runComputationM = interpretM $ \case
  p@(IsPrime i) -> do
    computation <- offloadComputation (Proxy :: Proxy Bool) computeIsPrime p i
    liftIO . logToiOS $ "Computation is: " ++ show computation
    setiOSLabel1 $ show computation
    pure computation
  p@(FactorialLength i) -> do
    computation <- offloadComputation (Proxy :: Proxy Int) computeFactorialLength p i
    liftIO . logToiOS $ "Computation is: " ++ show computation
    setiOSLabel2 $ show computation
    pure computation

runLocalComputationM :: forall effs a. LastMember IO effs => Eff (Computation ': effs) a -> Eff effs a
runLocalComputationM = interpretM $ \case
  (IsPrime i) -> do
    let computation = computeIsPrime i
    liftIO . logToiOS $ "Computation is: " ++ show computation
    setiOSLabel1 $ show computation
    setiOSIncLocalCounter
    pure computation
  (FactorialLength i) -> do
    let computation = computeFactorialLength i
    liftIO . logToiOS $ "Computation is: " ++ show computation
    setiOSLabel2 $ show computation
    setiOSIncLocalCounter
    pure computation
