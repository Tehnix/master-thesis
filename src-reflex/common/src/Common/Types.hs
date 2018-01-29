{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Types where

import Servant (FromHttpApiData, parseUrlPiece)


type Serialize a = Show a

serialize :: Show a => a -> String
serialize = show

data Operation next where
  WriteOutput :: Serialize a => a -> Operation ()

data Computation next where
  IsPrime :: Int -> Computation Bool
  FactorialLength :: Int -> Computation Int

-- TODO: Derive `ComputationS`, the `Show` instance, and `FromHttpApiData
-- from `Computation`, instead of this error-prone approach.

-- | Convert our `Computation` GADT into the corresponding `SComputation`.
instance Show (Computation n) where
  show (IsPrime i) = "IsPrimeS"
  show (FactorialLength i) = "FactorialLengthS"

-- | Server-side representation.
data ComputationS
  = IsPrimeS
  | FactorialLengthS

-- | Allow servent to decode the `SComputation` string into the constructor.
instance FromHttpApiData ComputationS where
  parseUrlPiece "IsPrimeS" = Right IsPrimeS
  parseUrlPiece "FactorialLengthS" = Right FactorialLengthS
  parseUrlPiece _ = Left "Error: Unknown"
