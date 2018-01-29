{-# LANGUAGE ConstraintKinds #-}
module Common.Types where

type Serialize a = Show a

data Operation next where
  WriteOutput :: Serialize a => a -> Operation ()

data Computation next where
  IsPrime :: Int -> Computation Bool
  FactorialLength :: Int -> Computation Int

instance Show (Computation n) where
  show (IsPrime i) = "IsPrime"
  show (FactorialLength i) = "FactorialLength"
