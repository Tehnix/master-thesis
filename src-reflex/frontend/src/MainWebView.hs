{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Language.Javascript.JSaddle.WKWebView as JSaddle
import Language.Javascript.JSaddle.WKWebView
import Reflex.Dom.Core (mainWidget)
import Reflex.Dom hiding (mainWidget)

main = JSaddle.run $ mainWidget $ el "div" $ text "Welcome to Reflex via JSaddle WebView"
