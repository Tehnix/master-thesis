{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}
module JavaScriptFFI where


#ifdef ghcjs_HOST_OS
import Data.JSString (JSString, pack)

foreign import javascript unsafe
  "if (typeof window.webkit === 'undefined') { window.webkit = {messageHandlers: {}}}"
  setupMessageHandler :: IO ()

foreign import javascript unsafe
  "if (typeof window.webkit.messageHandlers.log === 'undefined') { window.webkit.messageHandlers['log'] = {postMessage: console.log }}; window.webkit.messageHandlers.log.postMessage($1)"
  iOSLog :: JSString -> IO ()

foreign import javascript unsafe
  "if (typeof window.webkit.messageHandlers.labelOne === 'undefined') { window.webkit.messageHandlers['labelOne'] = {postMessage: console.log }}; window.webkit.messageHandlers.labelOne.postMessage($1)"
  iOSLabel1 :: JSString -> IO ()

foreign import javascript unsafe
  "if (typeof window.webkit.messageHandlers.labelTwo === 'undefined') { window.webkit.messageHandlers['labelTwo'] = {postMessage: console.log }}; window.webkit.messageHandlers.labelTwo.postMessage($1)"
  iOSLabel2 :: JSString -> IO ()

foreign import javascript unsafe
  "if (typeof window.webkit.messageHandlers.offloadCounter === 'undefined') { window.webkit.messageHandlers['offloadCounter'] = {postMessage: console.log }}; window.webkit.messageHandlers.offloadCounter.postMessage($1)"
  iOSOffloadCounter :: JSString -> IO ()

logToiOS :: String -> IO ()
logToiOS s = setupMessageHandler >> iOSLog (pack s)

setiOSLabel1 :: String -> IO ()
setiOSLabel1 s = setupMessageHandler >> iOSLabel1 (pack s)

setiOSLabel2 :: String -> IO ()
setiOSLabel2 s = setupMessageHandler >> iOSLabel2 (pack s)

setiOSIncOffloadCounter :: IO ()
setiOSIncOffloadCounter = setupMessageHandler >> iOSOffloadCounter "1"

setiOSIncLocalCounter :: IO ()
setiOSIncLocalCounter = setupMessageHandler >> iOSOffloadCounter "0"

#else
logToiOS :: String -> IO ()
logToiOS s = putStrLn s

setiOSLabel1 :: String -> IO ()
setiOSLabel1 s = putStrLn s

setiOSLabel2 :: String -> IO ()
setiOSLabel2 s = putStrLn s

setiOSIncOffloadCounter :: IO ()
setiOSIncOffloadCounter = pure ()

setiOSIncLocalCounter :: IO ()
setiOSIncLocalCounter = pure ()
#endif
