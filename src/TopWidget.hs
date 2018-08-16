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
main = debugAndWait 3911 $ mainWidget topWidget

debugAndWait p f = debug p f >> forever (threadDelay $ 1000 * 1000)

topWidget :: Widget t ()
topWidget = do
  text "Enter HTML here"

  ta <- divClass "" $ do
    textArea $ def &
      textAreaConfig_attributes .~ (constDyn ("rows" =: "20"))

  let
    v = parseToTokens <$> (value ta)
    d (Left e) = "Error in parse: " <> (T.intercalate ", " e)
    d (Right t) = T.pack $ render (renderReflexTree t)

  divClass "" $ do
    textArea $ def
      & textAreaConfig_attributes .~ (constDyn ("rows" =: "20"))
      & textAreaConfig_setValue .~ (updated (d . snd <$> v))

  elClass "pre" "" $ do
    dynText (d . snd <$> v)

  elClass "pre" "" $ do
    dynText ((TL.toStrict . pShowNoColor) <$> v)

