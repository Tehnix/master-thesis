module Main where

import Control.Monad.State
import qualified ManualFree as MF
import qualified DerivedFree as DF


manualProgram :: MF.Program ()
manualProgram = do
 filename <- MF.getInput
 contents <- MF.readFile filename
 MF.writeOutput contents
 result <- MF.computation 12 22
 MF.writeOutputInt result

derivedProgram :: DF.Program ()
derivedProgram = do
  filename <- DF.getInput
  contents <- DF.readFile filename
  DF.writeOutput contents
  result <- DF.computation 12 22
  DF.writeOutputInt result

main :: IO ()
main = do
  print "Manually setup free:"
  MF.testInterpreter manualProgram
  print "Derived setup free:"
  DF.testInterpreter derivedProgram
  (_, dfState) <- runStateT (DF.pureStateInterpreter derivedProgram) []
  print dfState
