{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Main where

import Servant
import Servant.API
import Network.Wai.Handler.Warp
import Data.Text
import Data.Proxy

import Common.Types
import Common.Computation


main :: IO ()
main = run 8080 (serve api server)

-- | Allow servent to decode the `Computation` string into the constructor. We supply a
-- throwaway value for the constructors to make them compile.
instance FromHttpApiData SComputation where
  parseUrlPiece "IsPrime" = Right SIsPrime
  parseUrlPiece "FactorialLength" = Right SFactorialLength
  parseUrlPiece _ = Left "Error: Unknown"

type OffloadApi
  = "off" :> Capture "f" SComputation :> Capture "v" Int :> Get '[PlainText] String

api :: Proxy OffloadApi
api = Proxy

server :: Server OffloadApi
server = compute
  where
    compute :: SComputation -> Int -> Handler String
    compute f val = return $ case f of
      SIsPrime -> show $ computeIsPrime val
      SFactorialLength -> show $ computeFactorialLength val
