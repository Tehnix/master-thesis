{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Language.Javascript.JSaddle.Warp as JSaddle
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core (mainWidget)
import Reflex.Dom hiding (mainWidget)

main = JSaddle.run 3709 $ mainWidget $ el "div" $ text "Welcome to Reflex via JSaddle Warp"
