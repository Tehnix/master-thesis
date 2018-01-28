{-# LANGUAGE FlexibleContexts #-}
module Common.Operations where

import Control.Monad.Freer

import Common.Types

readFilename :: Member Effect effs => String -> Eff effs String
readFilename = send . ReadFilename

writeOutput :: (Member Effect effs, Show a) => a -> Eff effs ()
writeOutput = send . WriteOutput

getInput :: Member Effect effs => Eff effs String
getInput = send GetInput

computation :: Member Effect effs => Int -> Int -> Eff effs Int
computation i1 i2 = send $ Computation i1 i2
