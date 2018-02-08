{-# LANGUAGE FlexibleContexts #-}
module Common.Operations where

import Control.Monad.Freer

import Common.Types


writeOutput :: (Member Operation effs, Serialize a) => a -> Eff effs ()
writeOutput = send . WriteOutput

isPrime :: Member Computation effs => Integer -> Eff effs Bool
isPrime = send . IsPrime

factorialLength :: Member Computation effs => Integer -> Eff effs Int
factorialLength = send . FactorialLength
