{-# LANGUAGE ConstraintKinds #-}
module Common.Types where


type Serialize a = Show a

data Effect next where
  ReadFilename :: String -> Effect String
  WriteOutput :: Show a => a -> Effect ()
  GetInput :: Effect String
  Computation :: Int -> Int -> Effect Int
