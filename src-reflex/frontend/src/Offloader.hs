module Offloader where

import Network.HTTP.Req
import Data.Default.Class
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Control.Exception as Error
import Control.Monad.IO.Class (liftIO)

import Profiler (shouldOffload)
import JavaScriptFFI (logToiOS, setiOSIncOffloadCounter, setiOSIncLocalCounter)


-- FIXME: Networking throws JS exception?

-- | Check if the code should be offloaded, returning the result
-- as a `Just String` if yes, and `Nothing` if no.
offload :: String -> String -> IO (Maybe String)
offload fun val = do
  decision <- shouldOffload
  logToiOS $ "Offloader decision: " ++ (show decision)
  if decision then do
    Error.handle handler $ do
      runReq def $ do
        -- Fetch the content.
          r <- req GET
                  (http "localhost" /: T.pack fun /: T.pack val)
                  NoReqBody
                  bsResponse
                  (port 8080)
          let content = C.unpack (responseBody r :: B.ByteString)
          -- Log the response and increment the offloder counter.
          liftIO $ logToiOS $ "Offloader got back response: " ++ content
          liftIO $ setiOSIncOffloadCounter
          pure $ Just content
  else do
    setiOSIncLocalCounter
    pure Nothing
  where
    -- Simply ignore any exception happening, instead return `Nothing`.
    handler :: HttpException -> IO (Maybe String)
    handler _ = do
      logToiOS $ "Offloader threw an exception"
      setiOSIncLocalCounter
      pure Nothing
