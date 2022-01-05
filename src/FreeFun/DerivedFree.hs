{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module DerivedFree where

import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.State


-- | The Effects model the possible operations we support. Because this is
-- not a Generalized algebraic data type (GADT), we end up needing to
-- pass around `next` (continuations).
data Effects next
  = ReadFile String (String -> next)
  | WriteOutput String next
  | WriteOutputInt Int next
  | GetInput (String -> next)
  | Computation Int Int (Int -> next)
  deriving (Functor) -- Automatically derive our functor definitions.

-- | Use Template Haskell to generate our function definitions.
makeFree ''Effects

type Program = Free Effects

-- | A simple interpreter for our program.
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

-- | A different interpreter that does not do any IO.
pureStateInterpreter :: MonadState [String] m => Program next -> m next
pureStateInterpreter (Pure a) = return a
pureStateInterpreter (Free effect) =
  case effect of
    ReadFile filename next ->
      let fakeFileContent = "Test file content for: " ++ filename
      in pureStateInterpreter $ next fakeFileContent
    WriteOutput s next -> do
      modify $ flip (++) [s]
      pureStateInterpreter next
    WriteOutputInt i next -> do
      modify $ flip (++) [show i]
      pureStateInterpreter next
    GetInput next -> let fakeInput = "Fake input"
      in pureStateInterpreter $ next fakeInput
    Computation i1 i2 next -> pureStateInterpreter $ next (i1 + i2)
