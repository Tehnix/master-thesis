{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module FreerFun where

import Control.Monad.Freer

type Serialize a = Show a

data Effect next where
  ReadFilename :: String -> Effect String
  WriteOutput :: Show a => a -> Effect ()
  GetInput :: Effect String
  Computation :: Int -> Int -> Effect Int


readFilename :: Member Effect effs => String -> Eff effs String
readFilename = send . ReadFilename

writeOutput :: (Member Effect effs, Show a) => a -> Eff effs ()
writeOutput = send . WriteOutput

getInput :: Member Effect effs => Eff effs String
getInput = send GetInput

computation :: Member Effect effs => Int -> Int -> Eff effs Int
computation i1 i2 = send $ Computation i1 i2

runEffect :: Eff '[Effect, IO] a -> IO a
runEffect = runM . interpretM (\case
  ReadFilename filename -> pure $ "Test file content for: " ++ filename
  WriteOutput s -> print s
  GetInput -> pure "Fake input"
  Computation i1 i2 -> pure $ i1 + i2)

program :: Eff '[Effect, IO] ()
program = do
 filename <- getInput
 contents <- readFilename filename
 writeOutput contents
 result <- computation 12 22
 writeOutput result

main :: IO ()
main = runEffect program
