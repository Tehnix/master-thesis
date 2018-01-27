{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Language.Javascript.JSaddle.WebKitGTK as JSaddle
import Language.Javascript.JSaddle.WebKitGTK
import Reflex.Dom.Core (mainWidget)
import Reflex.Dom hiding (mainWidget)

main :: IO ()
main = JSaddle.run 3911 $ mainWidget $ el "div" $ text "Welcome to Reflex via JSaddle WebkitGTK"
