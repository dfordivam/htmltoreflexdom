{-# language PartialTypeSignatures #-}
module RenderReflexToken (renderReflexTree) where

import ReflexToken

import Data.Text (Text)
import Data.Map (Map)
import Text.PrettyPrint
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe
import Data.Either
import qualified Data.Monoid as Monoid
import qualified Data.Map as Map
import Control.Monad.State
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup

renderReflexTree [] = text "No input"
renderReflexTree ts = vcat $ evalState (mapM renderRT ts) init
  where init = 1

type RM = State Int

txt = text . T.unpack
dq t = doubleQuotes (txt t)
constDyn d = parens (text "constDyn" <+> (parens d))

letBlk [] = empty
letBlk ls = text "let"
  $$ (nest 2 (vcat (map f ls)))
  where
    f (v, d) = v <+> "=" <+> d

(<--) :: Doc -> Doc -> Doc
(<--) a b = a <+> text "<-" <+> b

defOvrd :: [(Text, Doc)] -> Doc
defOvrd os = "def" $$ nest 0 (vcat $ map f os)
  where
    f (l, v) = if (isEmpty v)
      then empty
      else "&" <+> parens ((txt l) <+> ".~" <+> v)

do' a [] = a <+> "$ return ()"
do' a b = (a <+> "$ do") $$
  (nest 2 (vcat b $$
  text "return ()"))

getName t = do
  v <- state (\i -> (i, i+1))
  return $ text $ t ++ (show v)

getName2 t1 t2 = do
  v <- state (\i -> (i, i+1))
  return $ (text $ t1 ++ (show v), text $ t2 ++ (show v))

renderAttrs a = text (show a)

renderRT :: ReflexToken -> RM Doc
renderRT (RTText t) = return $ text "text" <+> (dq t)

renderRT (RTElement e a cs) = do
  (el, ls) <- case (Map.size a, Map.lookup "class" a) of
      (0, Nothing) -> return (text "el" <+> (dq e), [])
      (1, (Just c)) -> if e == "div"
         then return (text "divClass" <+> (dq c), [])
         else return (text "elClass" <+> (dq e) <+> (dq c), [])
      _ -> do
        attrName <- getName "attr"
        return (text "elAttr" <+> attrName, [(attrName, renderAttrs a)])

  es <- mapM renderRT cs
  return $
    letBlk ls $$
    (el `do'` es)

renderRT (RTInput (Area t) a) = do
  (ta, confName) <- getName2 "ta" "taConf"
  attrName <- getName "attr"
  let
    conf = defOvrd [("textAreaConfig_attributes", constDyn attrName)
                   , ("textAreaConfig_initialValue", v)]
    v = if (T.null t)
          then empty
          else (dq t)

    el = text "textArea" <+> confName
    ls = [(confName, conf), (attrName, renderAttrs a)]
  return $
    letBlk ls $$
    ta <-- el

renderRT (RTInput (TextInput tp ph) a) = do
  (ti, confName) <- getName2 "ti" "tiConf"
  attrName <- getName "attr"
  let
    conf = defOvrd [("textInputConfig_attributes", constDyn attrName)
                   , ("textInputConfig_inputType", it)
                   , ("textInputConfig_initialValue", iv)]

    it = maybe empty dq tp
    iv = maybe empty dq ph
    el = text "textArea" <+> confName
    ls = [(confName, conf), (attrName, renderAttrs a)]
  return $
    letBlk ls $$
    ti <-- el

renderRT (RTInput (CheckBox c) a) = do
  (cb, confName) <- getName2 "cb" "cbConf"
  attrName <- getName "attr"
  let
    conf = defOvrd [("checkboxConfig_attributes", constDyn attrName)]
    ls = [(confName, conf), (attrName, renderAttrs a)]
    checked = if c then text "True" else text "False"
    el = text "checkbox" <+> checked <+> confName
  return $
    letBlk ls $$
    cb <-- el

renderRT (RTInput (Range r) a) = do
  (ri, confName) <- getName2 "ri" "riConf"
  attrName <- getName "attr"
  let
    conf = defOvrd [("rangeInputConfig_attributes", constDyn attrName)
                   , ("rangeInputConfig_initialValue", iv)]
    iv = maybe empty (\t -> doubleQuotes (text (show t))) r
    ls = [(confName, conf), (attrName, renderAttrs a)]
    el = text "rangeInput" <+> confName
  return $
    letBlk ls $$
    ri <-- el

renderRT (RTInput (FileInput) a) = do
  (fi, confName) <- getName2 "fi" "fiConf"
  attrName <- getName "attr"
  let
    conf = defOvrd [("fileInputConfig_attributes", constDyn attrName)]
    ls = [(confName, conf), (attrName, renderAttrs a)]
    el = text "fileInput" <+> confName
  return $
    letBlk ls $$
    fi <-- el

renderRT (RTInput (DropDown sel opts) a) = do
  (dd, confName) <- getName2 "dd" "ddConf"
  attrName <- getName "attr"
  optMapName <- getName "optMap"

  let
    k = case (sel, opts) of
      (Just s, _) -> dq s
      (Nothing, ((k,_):_)) -> dq k

    optDoc = renderAttrs opts
    conf = defOvrd [("dropdownConfig_attributes", constDyn attrName)]
    ls = [(confName, conf), (attrName, renderAttrs a)
         , (optMapName, optDoc)]
    el = text "dropdown" <+> k <+> constDyn optMapName <+> confName
  return $
    letBlk ls $$
    dd <-- el

renderRT (RTInput (Button v) a) = do
  attrName <- getName "attr"
  (ev, eName) <- getName2 "btnEv" "btnEl"
  let
    ls = [(attrName, renderAttrs a)]
    el = text "elAttr'" <+> dq "button" <+> attrName
    e = parens (eName <+> ",_")

  es <- mapM renderRT v
  return $
    letBlk ls $$
    e <-- el `do'` es $$
    letBlk [(ev, text "domEvent Click" <> eName)]
