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

import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup

renderReflexTree [] = text "No input"
renderReflexTree ts = vcat $ map renderRT ts

txt = text . T.unpack
dq t = doubleQuotes (txt t)
constDyn d = parens (text "constDyn" <+> (parens d))

letBlk [] = empty
letBlk ls = text "let"
  $$ (nest 2 (vcat (map f ls)))
  where
    f (v, d) = v <+> "=" <+> d

renderRT (RTText t) = text "text" <+> (dq t)

renderRT (RTElement e a cs) =
  attrLet $$
  (el <+> attrName <+> do')
    $$ (nest 2 (vcat es))
  where
    es = map renderRT cs
    (el, attrName, attrLet) = case (Map.size a, Map.lookup "class" a) of
      (0, Nothing) -> (text "el" <+> (dq e), empty, empty)
      (1, (Just c)) -> if e == "div"
                       then (text "divClass" <+> (dq c), empty, empty)
                       else (text "elClass" <+> (dq e) <+> (dq c), empty, empty)
      _ -> (text "elAttr", attrName, letBlk [(attrName, text (show a))])
        where
          attrName = txt (T.append e "Attrs")

    do' = case cs of
      [] -> text "$ return ()"
      _ -> text "$ do"

renderRT (RTInput (Area t) a) =
  attrLet $$
  (el <+> attrName)
  where
    el = text "textArea"
    attrName = txt ("taConf")
    attrLet = text "let" <+> attrName <+> "="
      <+> "def & textAreaConfig_attributes .~"
      <+> constDyn (text (show a))
      <+> if (T.null t)
        then empty
        else "& textAreaConfig_initialValue .~" <+> (dq t)

renderRT (RTInput (TextInput tp ph) a) =
  attrLet $$
  (el <+> attrName)
  where
    el = text "textInput"
    attrName = txt ("tiConf")
    attrLet = text "let" <+> attrName <+> "="
      <+> text "def &" <+> parens ("textInputConfig_attributes .~"
        <+> constDyn (text (show a)))
      <+> maybe empty (\t -> text "&" <+> parens ("textInputConfig_inputType .~" <+> (dq t))) tp
      <+> maybe empty (\t -> text "&" <+> parens ("textInputConfig_initialValue .~" <+> (dq t))) ph


renderRT (RTInput (CheckBox c) a) =
  attrLet $$
  (el <+> checked <+> attrName)
  where
    checked = if c then text "True" else text "False"
    el = text "checkbox"
    attrName = txt ("cbConf")
    attrLet = text "let" <+> attrName <+> "="
      <+> text "def &" <+> parens ("checkboxConfig_attributes .~"
        <+> constDyn (text (show a)))

renderRT (RTInput (Range r) a) =
  attrLet $$
  (el <+> attrName)
  where
    el = text "rangeInput"
    attrName = txt ("riConf")
    attrLet = text "let" <+> attrName <+> "="
      <+> text "def &" <+> parens ("rangeInputConfig_attributes .~"
        <+> constDyn (text (show a)))
      <+> maybe empty (\t -> text "&" <+> parens ("rangeInputConfig_initialValue .~" <+> (doubleQuotes (text (show t))))) r

renderRT (RTInput (FileInput) a) =
  attrLet $$
  (el <+> attrName)
  where
    el = text "fileInput"
    attrName = txt ("fiConf")
    attrLet = text "let" <+> attrName <+> "="
      <+> text "def &" <+> parens ("fileInputConfig_attributes .~"
        <+> constDyn (text (show a)))

renderRT (RTInput (DropDown sel opts) a) =
  attrLet $$
  (el <+> k <+> dmap <+> conf)
  where
    el = text "dropdown"
    k = case (sel, opts) of
      (Just s, _) -> dq s
      (Nothing, ((k,_):_)) -> dq k

    dmap = txt ("ddVals")
    conf = txt ("ddConf")
    attrLet = letBlk [(conf, confVal), (dmap, dmapVal)]
    confVal = text "def &" <+> parens ("fileInputConfig_attributes .~"
        <+> constDyn (text (show a)))
    dmapVal = constDyn (text "fromList" <+> (text $ show opts))

renderRT (RTInput (Button v) a) =
  letBlk ls $$
  (el <+> attrName <+> do')
    $$ (nest 2 (renderReflexTree v))
  where
    el = text "elAttr'" <+> dq "button"
    attrName = txt ("btnAttrs")
    ls = (attrName, (text (show a)))
         : []
    do' = case v of
      [] -> text "$ return ()"
      _ -> text "$ do"
