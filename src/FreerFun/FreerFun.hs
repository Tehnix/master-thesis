{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module FreerFun where

import Control.Monad.Freer
import Control.Monad.Freer.TH


type Serialize a = Show a

-- | The Effects model the possible operations we support. We model the effects
-- as a Generalized algebraic data type (GADT) making our type signature read
-- much closer to a normal function signature, except for wrapping our return
-- types in `Effect`.
data Effect next where
  ReadFilename :: String -> Effect String
  WriteOutput :: Show a => a -> Effect ()
  GetInput :: Effect String
  Computation :: Int -> Int -> Effect Int

-- | This Template Haskell functions generates the code below.
makeEffect ''Effect

-- readFilename :: Member Effect effs => String -> Eff effs String
-- readFilename = send . ReadFilename

-- writeOutput :: (Member Effect effs, Show a) => a -> Eff effs ()
-- writeOutput = send . WriteOutput

-- getInput :: Member Effect effs => Eff effs String
-- getInput = send GetInput

-- computation :: Member Effect effs => Int -> Int -> Eff effs Int
-- computation i1 i2 = send $ Computation i1 i2


-- | Our interpreter that defines what happens when we run various
-- effects.
runEffect :: Eff '[Effect, IO] a -> IO a
runEffect = runM . interpretM (\case
  ReadFilename filename -> pure $ "Test file content for: " ++ filename
  WriteOutput s -> print s
  GetInput -> pure "Fake input"
  Computation i1 i2 -> do
    print "Imagine the offloading happening here"
    pure $ i1 + i2)

-- | Our program with our business logic.
program :: Eff '[Effect, IO] ()
program = do
 filename <- getInput
 contents <- readFilename filename
 writeOutput contents
 result <- computation 12 22
 writeOutput result

main :: IO ()
main = runEffect program
