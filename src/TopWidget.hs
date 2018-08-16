module TopWidget (topWidget) where

import Reflex.Dom.Core
import Language.Javascript.JSaddle.Warp
import Control.Concurrent
import Control.Monad
import Text.Pretty.Simple
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import ReflexToken
import RenderReflexToken
import Text.PrettyPrint (render)
import Data.Monoid

main :: IO ()
main = debugAndWait 3911 $
    mainWidgetWithHead'
      (const headWidget
      , const topWidget)

headWidget :: MonadWidget t m => m ()
headWidget = do
  let
  elAttr "meta" (("charset" =: "utf-8"))
    $ return ()

  elAttr "link"
    (("rel" =: "stylesheet")
      <> ("href" =: "https://cdn.rawgit.com/Chalarangelo/mini.css/v3.0.0/dist/mini-default.min.css"))
    $ return ()

debugAndWait p f = debug p f >> forever (threadDelay $ 1000 * 1000)

topWidget :: Widget t ()
topWidget = divClass "container" $ do
  divClass "header" $
    text "Convert HTML to Reflex-DOM Code"

  let
    d (Left e) = "Error in parse: " <> (T.intercalate ", " e)
    d (Right t) = T.pack $ render (renderReflexTree t)

  v <- divClass "row" $ do
    let taAttr ph = def &
          textAreaConfig_attributes .~ (constDyn
            (("rows" =: "20")
            <> ("placeholder" =: ph)
             <> ("style" =: "width: 100%;")))
    ta <- divClass "col-sm-6" $ do
      textArea $ taAttr "Enter HTML Here"

    let
      v = parseToTokens <$> (value ta)

    divClass "col-sm-6" $ do
      textArea $ taAttr "Reflex Code Output"
        & textAreaConfig_setValue .~ (updated (d . snd <$> v))
    return v

  let
    showDebug = divClass "row" $ do
      elClass "pre" "" $ do
        dynText ((TL.toStrict . pShowNoColor) <$> v)

  ev <- button "Show debug info"
  widgetHold (return ())
    (showDebug <$ ev)
  return ()

