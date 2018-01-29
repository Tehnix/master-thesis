{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Servant
import Servant.API
import Network.Wai.Handler.Warp
import Data.Text
import Data.Proxy

import Common.Types
import Common.Computation


type OffloadApi
  = "off" :> Capture "f" String :> Capture "v" Int :> Get '[PlainText] String

api :: Proxy OffloadApi
api = Proxy

main :: IO ()
main = run 8080 (serve api server)

server :: Server OffloadApi
server = compute
  where
    compute :: String -> Int -> Handler String
    compute f val = pure $ case f of
        "IsPrime" -> show $ computeIsPrime val
        "FactorialLength" -> show $ computeFactorialLength val
        _ -> "Error: Invalid function"
