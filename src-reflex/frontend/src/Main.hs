{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Concurrent.Async.Lifted.Safe
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import Reflex.Dom.Core (mainWidgetWithHead)

import UI
import Interpreter
import JavaScriptFFI (logToiOS, setiOSLabel1, setiOSLabel2)
import Common.Operations
import Common.Types
import Control.Monad.Freer


main :: IO ()
main = do
  -- logToiOS "Initializing app"
  setiOSLabel1 ""
  setiOSLabel2 ""
  -- Run both our Freer program and our FRP UI concurrently.
  concurrently_
    (runProgramM program)
    (JSaddle.run 3710 $ mainWidgetWithHead headElem bodyElem)

-- | Our program.
program :: Eff '[Operation, Computation, IO] ()
program = do
 fac <- factorialLength 22
 writeOutput fac
 results <- isPrime 12
 anotherResult <- isPrime 29
 mapComputation factorialLength [1..10000]
 mapComputation isPrime [1, 33, 99, 192]
 writeOutput results
 writeOutput anotherResult

-- | Map a computation over a list of values.
mapComputation :: (Member Operation r, Member Computation r, Member IO r, Show b)
               => (a -> Eff r b) -> [a] -> Eff r ()
mapComputation _ [] = pure ()
mapComputation f (i:is) = do
  result <- f i
  writeOutput result
  mapComputation f is
