{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}
module JavaScriptFFI where


#ifdef ghcjs_HOST_OS
import Data.JSString (JSString, pack)

foreign import javascript unsafe
  "if (typeof window.webkit === 'undefined') { window.webkit = {messageHandlers: {log: {postMessage: console.log }}}}; window.webkit.messageHandlers.log.postMessage($1)"
  iOSLog :: JSString -> IO ()

foreign import javascript unsafe
  "if (typeof window.webkit === 'undefined') { window.webkit = {messageHandlers: {log: {postMessage: console.labelOne }}}}; window.webkit.messageHandlers.labelOne.postMessage($1)"
  iOSLabel1 :: JSString -> IO ()

foreign import javascript unsafe
  "if (typeof window.webkit === 'undefined') { window.webkit = {messageHandlers: {log: {postMessage: console.labelTwo }}}}; window.webkit.messageHandlers.labelTwo.postMessage($1)"
  iOSLabel2 :: JSString -> IO ()

logToiOS :: String -> IO ()
logToiOS s = iOSLog (pack s)

setiOSLabel1 :: String -> IO ()
setiOSLabel1 s = iOSLabel1 (pack s)

setiOSLabel2 :: String -> IO ()
setiOSLabel2 s = iOSLabel2 (pack s)
#else
logToiOS :: String -> IO ()
logToiOS s = putStrLn s

setiOSLabel1 :: String -> IO ()
setiOSLabel1 s = putStrLn s

setiOSLabel2 :: String -> IO ()
setiOSLabel2 s = putStrLn s
#endif
