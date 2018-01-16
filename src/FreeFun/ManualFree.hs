module ManualFree where

import Control.Monad.Free


data Effects next
  = ReadFile String (String -> next)
  | WriteOutput String next
  | WriteOutputInt Int next
  | GetInput (String -> next)
  | Computation Int Int (Int -> next)

instance Functor Effects where
  fmap f (ReadFile s g) = ReadFile s (f . g)
  fmap f (WriteOutput s g) = WriteOutput s (f g)
  fmap f (WriteOutputInt i g) = WriteOutputInt i (f g)
  fmap f (GetInput g) = GetInput (f . g)
  fmap f (Computation i1 i2 g) = Computation i1 i2 (f . g)

type Program = Free Effects

readFile :: String -> Program String
readFile s = liftF $ ReadFile s id

writeOutput :: String -> Program ()
writeOutput s = liftF $ WriteOutput s ()

writeOutputInt :: Int -> Program ()
writeOutputInt i = liftF $ WriteOutputInt i ()

getInput :: Program String
getInput = liftF $ GetInput id

computation :: Int -> Int -> Program Int
computation i1 i2 = liftF $ Computation i1 i2 id

testInterpreter :: Program next -> IO next
testInterpreter (Pure a) = return a
testInterpreter (Free effect) =
  case effect of
    ReadFile filename next -> do
      let fakeFileContent = "Test file content for: " ++ filename
      testInterpreter $ next fakeFileContent
    WriteOutput s next -> do
      putStrLn s
      testInterpreter next
    WriteOutputInt i next -> do
      print i
      testInterpreter next
    GetInput next -> do
      let fakeInput = "Fake input"
      testInterpreter $ next fakeInput
    Computation i1 i2 next -> testInterpreter $ next (i1 + i2)
