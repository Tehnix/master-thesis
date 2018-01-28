module Main where

import qualified Language.Javascript.JSaddle.Warp as JSaddle
import Reflex.Dom.Core (mainWidgetWithHead)
import Reflex.Dom hiding (mainWidgetWithHead)
import Data.Map as Map
import Control.Monad.Freer
import Data.Monoid

import Common.Operations
import Common.Types

main :: IO ()
main = do
  runEffect program
  JSaddle.run 3709 $ mainWidgetWithHead headElem bodyElem

headElem :: MonadWidget t m => m ()
headElem = do
  el "title" $ text "Offie The Offloader"
  elAttr "meta" (Map.fromList [("charset", "UTF-8")]) $ return ()
  styleSheet "styles.css"
  where
    styleSheet name = elAttr "link" (
          ("rel" =: "stylesheet")
        <> ("type" =: "text/css")
        <> ("href" =: name))
        blank

bodyElem :: MonadWidget t m => m ()
bodyElem = elClass "div" "main-component" $ do
    el "h1" $ text "Welcome to Reflex via JSaddle Warp!"
    el "p" $ text "Ready?"
    blank


runEffect :: (Monad m) => Eff '[Effect, m] a -> m a
runEffect = runM . interpretM (\case
  ReadFilename filename -> pure $ "Test file content for: " ++ filename
  WriteOutput _ -> do
    -- print s
    pure ()
  GetInput -> pure "Fake input"
  Computation i1 i2 -> do
    -- print "Imagine the offloading happening here"
    pure $ i1 + i2)

program :: Eff '[Effect, IO] ()
program = do
 filename <- getInput
 contents <- readFilename filename
 writeOutput contents
 result <- computation 12 22
 writeOutput result
