module TopWidget (topWidget) where

import Reflex.Dom
import Control.Concurrent
import Control.Monad
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import ReflexToken
import RenderReflexToken
import Text.PrettyPrint (render)
import Data.Monoid

-- import Text.Pretty.Simple
-- import Language.Javascript.JSaddle.Warp

-- debugAndWait p f = debug p f >> forever (threadDelay $ 1000 * 1000)

topWidget :: IO ()
topWidget =
  -- debugAndWait 3911 $
    mainWidgetWithHead'
      (const headWidget
      , const bodyWidget)

headWidget :: MonadWidget t m => m ()
headWidget = do
  let
  elAttr "meta" (("charset" =: "utf-8"))
    $ return ()

  elAttr "link"
    (("rel" =: "stylesheet")
      <> ("href" =: "https://cdn.rawgit.com/Chalarangelo/mini.css/v3.0.0/dist/mini-default.min.css"))
    $ return ()


bodyWidget :: Widget t ()
bodyWidget = divClass "container" $ do
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
        -- dynText ((TL.toStrict . pShowNoColor) <$> v)
        dynText (tshow <$> v)

  ev <- button "Show debug info"
  widgetHold (return ())
    (showDebug <$ ev)
  return ()

