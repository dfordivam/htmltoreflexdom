{-# language PatternSynonyms #-}
module ReflexToken where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe
import Data.Either
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.List as List
import Text.Read (readMaybe)
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup

data ReflexToken =
    RTText Text
  | RTElement Text (Map Text Text) [ReflexToken]
  | RTInput InputType (Map Text Text)
  deriving (Eq, Ord, Show)

data InputType =
  TextInput (Maybe Text) (Maybe Text)
  | Area Text
  | Range (Maybe Float)
  | CheckBox Bool
  | FileInput
  | DropDown (Maybe Text) [(Text, Text)]
  | Button ([ReflexToken])
  deriving (Eq, Ord, Show)

pattern TextArea1 a t = (TagBranch "textarea" a [(TagLeaf (TagText t))])
pattern TextArea2 a = (TagBranch "textarea" a [])

pattern InputElement1 a = (TagBranch "input" a [])

pattern SelectElement1 a opts = (TagBranch "select" a opts)
pattern OptionElement1 a t = (TagBranch "option" a [(TagLeaf (TagText t))])

pattern ButtonElement1 a e = (TagBranch "button" a e)

parseToTokens :: Text -> ([TagTree Text], Either [Text] [ReflexToken])
parseToTokens inp = (tree, toRT tree)
  where
    tree = transformTree transformF $ parseTree inp

    toRT ts = case (lefts es) of
                [] -> Right $ catMaybes (rights es)
                ls -> Left (concat ls)
      where es = map toRT' ts

    toRT' :: TagTree Text -> Either [Text] (Maybe ReflexToken)

    toRT' (TextArea1 a t) = Right $ Just $
      RTInput (Area t) (Map.fromList a)
    toRT' (TextArea2 a) = Right $ Just $
      RTInput (Area "") (Map.fromList a)
    toRT' (InputElement1 a) = getInputRT a

    toRT' (ButtonElement1 a e) =
      (\e' -> Just $ RTInput (Button e') (Map.fromList a)) <$> (toRT e)

    toRT' (SelectElement1 a []) = Left ["Select without option elements"]
    toRT' (SelectElement1 a opts) = getSelectOpts a opts

    toRT' (TagBranch e a cs) = case (toRT cs) of
      (Right es) -> Right $ Just $
        RTElement e (Map.fromList a) es
      (Left ls) -> Left ls

    toRT' (TagLeaf (TagText t)) = Right $ Just $ RTText t

    toRT' (TagLeaf (TagOpen e a)) = case e of
      "input" -> getInputRT a

    toRT' (TagLeaf (TagComment _)) = Right $ Nothing
    toRT' (TagLeaf (TagClose e)) = Left $
      ["Misplaced tag?: " <> e]
    toRT' (TagLeaf (TagWarning e)) = Left [e]
    toRT' (TagLeaf (TagPosition r c)) = Left
     ["Row:" <> (tshow r) <> ", "
      <> "Col:" <> (tshow c) ]

getInputRT a = case (List.lookup "type" a) of
  (Just "button") -> Right $ Just $
    RTInput (Button v) (Map.fromList a2)
    where
      v = maybe [] ((:[]) . RTText) $ List.lookup "value" a
      a2 = filter (\(k,_) -> not ((k == "type") || (k == "value"))) a

  (Just "checkbox") -> Right $ Just $
    RTInput (CheckBox c) (Map.fromList a2)
    where
      c = maybe False (const True) $ List.lookup "checked" a
      a2 = filter (\(k,_) -> not ((k == "type") || (k == "checked"))) a

  (Just "range") -> Right $ Just $
    RTInput (Range r) (Map.fromList a2)
    where
      r = readMaybe . T.unpack =<< List.lookup "value" a
      a2 = filter (\(k,_) -> not ((k == "type") || (k == "value"))) a

  (Just "file") -> Right $ Just $
    RTInput (FileInput) (Map.fromList a2)
    where
      a2 = filter (((/=) "type") . fst) a

  (Just t) -> Right $ Just $
    RTInput (TextInput t1 ph) (Map.fromList a2)
    where
      t1 = if t == "text" then Nothing else Just t
      ph = List.lookup "placeholder" a
      a2 = filter (\(k,_) -> not ((k == "type") || (k == "placeholder"))) a

getSelectOpts a os = case (lefts os2) of
  [] -> Right $ Just $
    RTInput (DropDown sel opts) (Map.fromList a)
  es -> Left es
  where
    sel = case (filter fst $ rights os2) of
      ((_,(k,_)):_) -> Just k
      _ -> Nothing
    os2 = map getOpt os
    opts = map snd $ rights os2
    getOpt (OptionElement1 a t) = Right (s, (v,t))
      where v = maybe "" id (List.lookup "value" a)
            s = maybe False (const True) (List.lookup "selected" a)
    getOpt _ = Left "Invalid Select option value"

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- Make all tags names, and attribute keys lowercase
-- Remove whitespace text nodes, and strip text
transformF (TagBranch t a ts) = [TagBranch (T.toCaseFold t) a ts]
  where
    anew = map (\(k,v) -> (T.toCaseFold k, v)) a

transformF a@(TagLeaf t) = case t of
  (TagText t) -> if T.null (T.strip t) then [] else [TagLeaf (TagText (T.strip t))]
  (TagOpen t a) -> [TagLeaf (TagOpen (T.toCaseFold t) anew)]
    where
      anew = map (\(k,v) -> (T.toCaseFold k, v)) a
  _ -> [a]
