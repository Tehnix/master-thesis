{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Control.Monad.Freer

import Common.Types
import Common.Computation
import JavaScriptFFI
import Network.Wreq
import Control.Lens
import Profiler
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy.Char8 as C

offload :: String -> String -> IO String
offload fun val = do
  r <- get url
  pure . C.unpack $ r ^. responseBody
  where url = "http://localhost:8080/off?f=" ++ fun ++ "&v=" ++ val

runProgramM :: Eff '[Operation, Computation, IO] a -> IO a
runProgramM p = runM . runComputationM . runEventM $ p

runEventM :: forall effs a. LastMember IO effs => Eff (Operation ': effs) a -> Eff effs a
runEventM = interpretM $ \case
  WriteOutput s -> logToiOS (show s)

runComputationM :: forall effs a. LastMember IO effs => Eff (Computation ': effs) a -> Eff effs a
runComputationM = interpretM $ \case
  IsPrime i -> do
    shouldOffload' <- shouldOffload
    if shouldOffload'
    then do
      c <- offload (show (IsPrime i)) (show i)
      setiOSLabel1 c
      case (readMaybe c) :: Maybe Bool of
        Nothing -> pure False
        Just b -> pure b
    else do
      let c = computeIsPrime i
      setiOSLabel1 (show c)
      pure c
  FactorialLength i -> do
    shouldOffload' <- shouldOffload
    if shouldOffload'
    then do
      c <- offload (show (FactorialLength i)) (show i)
      setiOSLabel2 c
      case (readMaybe c) :: Maybe Int of
        Nothing -> pure 0
        Just i -> pure i
    else do
      let c = computeFactorialLength i
      setiOSLabel2 (show c)
      pure c
