{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
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


type OffloadApi
  = "off" :> Capture "f" ComputationS :> Capture "v" Int :> Get '[PlainText] String

api :: Proxy OffloadApi
api = Proxy

server :: Server OffloadApi
server = compute
  where
    compute :: ComputationS -> Int -> Handler String
    compute f val = pure $ case f of
      IsPrimeS -> show $ computeIsPrime val
      FactorialLengthS -> show $ computeFactorialLength val
