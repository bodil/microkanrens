module Kanren.Value where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (class Foldable, foldr)
import Data.List as List



data LogicValue = LVar Int
                | Int Int
                | String String
                | Pair LogicValue LogicValue
                | Empty



cons :: ∀ a b. (AsLogicValue a, AsLogicValue b) ⇒ a → b → LogicValue
cons a b = Pair (quote a) (quote b)

infixr 6 cons as :



instance eqLogicValue :: Eq LogicValue where
  eq (LVar a) (LVar b) = a == b
  eq (Int a) (Int b) = a == b
  eq (String a) (String b) = a == b
  eq (Pair a b) (Pair a' b') = a == b && a' == b'
  eq Empty Empty = true
  eq _ _ = false



isProperList :: LogicValue → Boolean
isProperList (Pair car cdr) = isProperList cdr
isProperList Empty = true
isProperList _ = false

toArray :: LogicValue → Maybe (Array LogicValue)
toArray l@(Pair _ _) | isProperList l =
  Just $ List.toUnfoldable $ convert l
  where convert (Pair car cdr) = List.Cons car $ convert cdr
        convert _ = List.Nil
toArray _ = Nothing

instance showLogicValue :: Show LogicValue where
  show (LVar i) = "_." ++ show i
  show (Int i) = show i
  show (String s) = s
  show p@(Pair _ _) | isProperList p = case toArray p of
    Just l → show l
    Nothing → "bad list"
  show (Pair car cdr) = "(" ++ show car ++ " . " ++ show cdr ++ ")"
  show Empty = "[]"



class AsLogicValue a where
  quote :: a → LogicValue

instance logicValueAsLogicValue :: AsLogicValue LogicValue where
  quote = id

instance intAsLogicValue :: AsLogicValue Int where
  quote = Int

instance stringAsLogicValue :: AsLogicValue String where
  quote = String

instance foldableAsLogicValue :: (AsLogicValue a, Foldable f) ⇒ AsLogicValue (f a) where
  quote = foldr (\car cdr → Pair (quote car) cdr) Empty
