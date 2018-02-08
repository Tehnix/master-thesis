module Main where

import Control.Concurrent.Async.Lifted.Safe
import qualified Language.Javascript.JSaddle.WKWebView as JSaddle
import Reflex.Dom.Core (mainWidgetWithHead)

import UI
import Interpreter
import JavaScriptFFI (logToiOS, setiOSLabel1, setiOSLabel2)
import Common.Operations
import Common.Types
import Control.Monad.Freer


main :: IO ()
main = do
  logToiOS "Initializing app"
  setiOSLabel1 ""
  setiOSLabel2 ""
  concurrently_
    (runProgramM program)
    (JSaddle.run  $ mainWidgetWithHead headElem bodyElem)

program :: Eff '[Operation, Computation, IO] ()
program = do
  fac <- factorialLength 22
  writeOutput fac
  result <- isPrime 12
  writeOutput result
