{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Common.Types where


type Serialize a = Show a

serialize :: Show a => a -> String
serialize = show

data Operation next where
  WriteOutput :: Serialize a => a -> Operation ()

data Computation next where
  IsPrime :: Integer -> Computation Bool
  FactorialLength :: Integer -> Computation Int

-- TODO: Derive `ComputationS`, the `Show` instance, and `FromHttpApiData
-- from `Computation`, instead of this error-prone approach.

data SComputation
  = SIsPrime
  | SFactorialLength

-- | Convert our `Computation` GADT into the corresponding `SComputation`.
instance Show (Computation n) where
  show (IsPrime i) = "IsPrime"
  show (FactorialLength i) = "FactorialLength"
