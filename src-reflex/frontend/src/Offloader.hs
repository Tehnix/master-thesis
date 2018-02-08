module Offloader where

import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import qualified JavaScript.XHR as XHR
import Data.Monoid ((<>))

import Profiler (shouldOffload)
import JavaScriptFFI (logToiOS, setiOSIncOffloadCounter, setiOSIncLocalCounter)


-- | Check if the code should be offloaded, returning the result
-- as a `Just String` if yes, and `Nothing` if no.
offload :: String -> String -> IO (Maybe String)
offload fun val = do
  decision <- shouldOffload
  logToiOS $ "Offloader decision: " <> show decision
  if decision then do
      let url = "http://localhost:8080/off/" <> T.pack fun <> "/" <> T.pack val
      (code, resp) <- XHR.get url
      -- Log the response and increment the offloder counter.
      liftIO . logToiOS $ "Offloading to url: " <> T.unpack url
      liftIO . logToiOS $ "Offloader got back response code: " <> show code
      liftIO . logToiOS $ "Offloader got back response: " <> T.unpack resp
      liftIO setiOSIncOffloadCounter
      pure . Just . T.unpack $ resp
  else setiOSIncLocalCounter >> pure Nothing
