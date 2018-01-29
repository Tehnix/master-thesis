module UI where
import Reflex.Dom
import Data.Map as Map
import Data.Monoid


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
  rec
    el "h1" $ text "Welcome to Offie The Offloader"
    el "p" $ text "Tests will start automatically."
  blank
