module Kanren where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.List as List
import Data.List.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)

type Stream a = Lazy.List a

data LogicValue = LVar Int
                | Int Int
                | String String
                | Pair LogicValue LogicValue
                | Empty

type State = Map Int LogicValue

data SC = SC State Int

type Goal = SC → Stream SC

class AsLogicValue a where
  quote :: a → LogicValue

emptyState :: SC
emptyState = SC Map.empty 0

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

instance showSC :: Show SC where
  show (SC s c) = "state=" ++ show s ++ ", counter=" ++ show c

instance logicValueAsLogicValue :: AsLogicValue LogicValue where
  quote = id

instance intAsLogicValue :: AsLogicValue Int where
  quote = Int

instance stringAsLogicValue :: AsLogicValue String where
  quote = String

instance foldableAsLogicValue :: (AsLogicValue a, Foldable f) ⇒ AsLogicValue (f a) where
  quote = foldr (\car cdr -> Pair (quote car) cdr) Empty

walk :: LogicValue → State → LogicValue
walk k@(LVar l) s  = case Map.lookup l s of
  Just v → walk v s
  Nothing → k
walk k _ = k

unify :: LogicValue → LogicValue → State → Maybe State
unify l r s = unify' l r $ Just s
  where
    unify' :: LogicValue → LogicValue → Maybe State → Maybe State
    unify' _ _ Nothing = Nothing
    unify' l r (Just s) = case (walk l s), (walk r s) of
      (LVar l), (LVar r) | l == r → Just s
      (LVar l), r → Just $ Map.insert l r s
      l, (LVar r) → Just $ Map.insert r l s
      (Pair l ls), (Pair r rs) → unify' ls rs (unify' l r (Just s))
      l, r | l == r → Just s
      _, _ → Nothing

infixl 4 equals as ?==

equals :: ∀ a b. (AsLogicValue a, AsLogicValue b) ⇒ a → b → Goal
equals l r = \(SC s c) → case unify (quote l) (quote r) s of
  Nothing → mempty
  Just s' → return $ SC s' c

fresh :: (LogicValue → Goal) → Goal
fresh f = \(SC s c) → (f (LVar c)) $ SC s (c + 1)

fresh2 :: (LogicValue → LogicValue → Goal) → Goal
fresh2 f = \(SC s c) → (f (LVar c) (LVar (c + 1))) $ SC s (c + 2)

fresh3 :: (LogicValue → LogicValue → LogicValue → Goal) → Goal
fresh3 f = \(SC s c) → (f (LVar c) (LVar (c + 1)) (LVar (c + 2))) $ SC s (c + 3)

infixl 3 disjo as ?||
disjo :: Goal → Goal → Goal
disjo a b = \sc → a sc <> b sc

infixl 3 conjo as ?&&
conjo :: Goal → Goal → Goal
conjo a b = \sc → a sc >>= b

callGoal :: Goal → Stream SC
callGoal g = g emptyState

pull :: Int → Goal → Stream SC
pull n = callGoal >>> Lazy.take n

walk_ :: LogicValue → State → LogicValue
walk_ v s = case walk v s of
  Pair car cdr -> Pair (walk_ car s) (walk_ cdr s)
  v → v

reify1st :: SC → LogicValue
reify1st (SC s _) = walk_ (LVar 0) s

run' :: Int → Goal → Array LogicValue
run' n = pull n >>> map reify1st >>> Lazy.toUnfoldable

run :: Goal → Array LogicValue
run = callGoal >>> map reify1st >>> Lazy.toUnfoldable

infixl 0 run' as <?

appendo :: ∀ a b c. (AsLogicValue a, AsLogicValue b, AsLogicValue c) ⇒ a → b → c → Goal
appendo l r out = appendo' (quote l) (quote r) (quote out)
  where appendo' l r out = (l ?== Empty ?&& r ?== out)
                           ?|| (fresh3 \a d res →
                                 Pair a d ?== l
                                 ?&& Pair a res ?== out
                                 ?&& appendo' d r res)
