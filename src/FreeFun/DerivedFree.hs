{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
module DerivedFree where

import Control.Monad.State
import Control.Monad.Free
import Control.Monad.Free.TH


data Effects next
  = ReadFile String (String -> next)
  | WriteOutput String next
  | WriteOutputInt Int next
  | GetInput (String -> next)
  | Computation Int Int (Int -> next)
  deriving (Functor)

makeFree ''Effects

type Program = Free Effects

testInterpreter :: Program next -> IO next
testInterpreter (Pure a) = return a
testInterpreter (Free effect) =
  case effect of
    ReadFile filename next ->
      let fakeFileContent = "Test file content for: " ++ filename
      in testInterpreter $ next fakeFileContent
    WriteOutput s next -> putStrLn s >> testInterpreter next
    WriteOutputInt i next -> print i >> testInterpreter next
    GetInput next -> let fakeInput = "Fake input"
      in testInterpreter $ next fakeInput
    Computation i1 i2 next -> testInterpreter $ next (i1 + i2)

pureStateInterpreter :: MonadState [String] m => Program next -> m next
pureStateInterpreter (Pure a) = return a
pureStateInterpreter (Free effect) =
  case effect of
    ReadFile filename next -> do
      let fakeFileContent = "Test file content for: " ++ filename
      pureStateInterpreter $ next fakeFileContent
    WriteOutput s next -> do
      modify $ flip (++) [s]
      pureStateInterpreter next
    WriteOutputInt i next -> do
      modify $ flip (++) [show i]
      pureStateInterpreter next
    GetInput next -> do
      let fakeInput = "Fake input"
      pureStateInterpreter $ next fakeInput
    Computation i1 i2 next -> pureStateInterpreter $ next (i1 + i2)
