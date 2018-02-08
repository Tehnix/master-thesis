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
import Network.Wai
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Data.Text
import Data.Proxy
import Control.Monad.IO.Class (liftIO)

import Common.Types
import Common.Computation


type OffloadApi
  = "off" :> Capture "f" SComputation :> Capture "v" Integer :> Get '[JSON, PlainText] String

server :: Server OffloadApi
server = compute
  where
    compute :: SComputation -> Integer -> Handler String
    compute f val = case f of
      SIsPrime -> pure . show $ computeIsPrime val
      SFactorialLength -> pure . show $ computeFactorialLength val

-- | Allow servent to decode the `Computation` string into the constructor. We supply a
-- throwaway value for the constructors to make them compile.
instance FromHttpApiData SComputation where
  parseUrlPiece "IsPrime" = Right SIsPrime
  parseUrlPiece "FactorialLength" = Right SFactorialLength
  parseUrlPiece _ = Left "Error: Unknown"

api :: Proxy OffloadApi
api = Proxy

main :: IO ()
main = do
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings $ corsWithContentType $ provideOptions api $ (serve api server)
  where
    -- We need to allow both CORS and get servant to respond to OPTIONS.
    corsWithContentType :: Middleware
    corsWithContentType = cors (const $ Just policy)
        where
          policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"] }
