module Main where

import Control.Concurrent.Async.Lifted.Safe
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import Reflex.Dom.Core (mainWidgetWithHead)

import UI
import Interpreter
import Common.Operations
import Common.Types
import Control.Monad.Freer


main :: IO ()
main = do
  concurrently_
    (runProgramM program)
    (JSaddle.run 3709 $ mainWidgetWithHead headElem bodyElem)

program :: Eff '[Operation, Computation, IO] ()
program = do
 fac <- factorialLength 22
 writeOutput fac
 result <- isPrime 12
 writeOutput result
