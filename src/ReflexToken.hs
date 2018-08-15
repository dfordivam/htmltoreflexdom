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
  | Range Float
  | CheckBox Bool
  | FileInput
  | DropDown
  | Button
  deriving (Eq, Ord, Show)

pattern TextArea1 a t = (TagBranch "textarea" a [(TagLeaf (TagText t))])
pattern TextArea2 a = (TagBranch "textarea" a [])

pattern TextInput1 a = (TagBranch "input" a [])

parseToTokens :: Text -> ([TagTree Text], Either [Text] [ReflexToken])
parseToTokens inp = (tree, toRT tree)
  where
    tree = parseTree inp

    toRT ts = case (lefts es) of
                [] -> Right $ catMaybes (rights es)
                ls -> Left (concat ls)
      where es = map toRT' ts

    toRT' :: TagTree Text -> Either [Text] (Maybe ReflexToken)

    toRT' (TextArea1 a t) = Right $ Just $
      RTInput (Area t) (Map.fromList a)
    toRT' (TextArea2 a) = Right $ Just $
      RTInput (Area "") (Map.fromList a)
    toRT' (TextInput1 a) = getInputRT a

    toRT' (TagBranch e a cs) = case (toRT cs) of
      (Right es) -> Right $ Just $
        RTElement e (Map.fromList a) es
      (Left ls) -> Left ls

    toRT' (TagLeaf (TagText t))
      | T.null (T.strip t) = Right $ Nothing
      | otherwise = Right $ Just $ RTText (T.strip t)

    toRT' (TagLeaf (TagOpen e a)) = case e of
      "input" -> getInputRT a

        -- _ -> Right $ Just $
        --   RTElement e (Map.fromList a) []
      -- _ -> Right $ Just $
      --   RTElement e (Map.fromList a) []

    toRT' (TagLeaf (TagComment _)) = Right $ Nothing
    toRT' (TagLeaf (TagClose e)) = Left $
      ["Misplaced tag?: " <> e]
    toRT' (TagLeaf (TagWarning e)) = Left [e]
    toRT' (TagLeaf (TagPosition r c)) = Left
     ["Row:" <> (tshow r) <> ", "
      <> "Col:" <> (tshow c) ]

getInputRT a = case (List.lookup "type" a) of
  (Just "checkbox") -> Right $ Just $
    RTInput (CheckBox c) (Map.fromList a2)
    where
      c = maybe False (const True) $ List.lookup "checked" a
      a2 = filter (\(k,_) -> not ((k == "type") || (k == "checked"))) a

  (Just t) -> Right $ Just $
    RTInput (TextInput t1 ph) (Map.fromList a2)
    where
      t1 = if t == "text" then Nothing else Just t
      ph = List.lookup "placeholder" a
      a2 = filter (\(k,_) -> not ((k == "type") || (k == "placeholder"))) a

tshow :: (Show a) => a -> Text
tshow = T.pack . show
