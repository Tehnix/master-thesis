module Offloader where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import qualified JavaScript.XHR as XHR
import Data.Monoid ((<>))

import Common.Types
import Text.Read (readMaybe)
import Profiler (shouldOffload)
import JavaScriptFFI (logToiOS, setiOSIncOffloadCounter, setiOSIncLocalCounter)

remoteUrl :: Text
remoteUrl = "10.0.2.7:8080"

localUrl :: Text
localUrl = "localhost:8080"

-- | Check if the code should be offloaded, returning the result
-- as a `Just String` if yes, and `Nothing` if no.
offload :: String -> String -> IO (Maybe String)
offload fun val = do
  decision <- shouldOffload
  let url = "http://" <> remoteUrl <> "/off/" <> T.pack fun <> "/" <> T.pack val
  logToiOS $ "Offloader decision: " <> show decision
  if decision
    then do
      (code, resp) <- XHR.get url
      -- Log the response and increment the offloder counter.
      liftIO . logToiOS $ "Offloading to url: " <> T.unpack url
      liftIO . logToiOS $ "Offloader got back response code: " <> show code
      liftIO . logToiOS $ "Offloader got back response: " <> T.unpack resp
      liftIO setiOSIncOffloadCounter
      if code == 200
        then pure . Just . T.unpack $ resp
        else setiOSIncLocalCounter >> pure Nothing
    else setiOSIncLocalCounter >> pure Nothing

-- | Offload a computation, by taking
--   - a proxy type that specifies the output type
--   - the fallback function,
--   - the Computation type,
--   - the input,
-- and return the output.
offloadComputation :: forall proxy a b c. (Read a, Show b, Show c)
                   => proxy a -> (c -> a) -> b -> c -> IO a
offloadComputation _ f p i = do
  mComputation <- offload (serialize p) (serialize i)
  pure $ case mComputation of
    Nothing -> f i
    Just mSC ->  maybe (f i) id ((readMaybe (read mSC :: String)) :: Maybe a)
