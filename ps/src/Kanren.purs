module Kanren where

import Prelude

import Data.Foldable (class Foldable)
import Data.List (List(Cons))
import Data.List as List
import Data.List.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(Tuple))

type Stream a = Lazy.List a

data LogicValue = LVar Int
                | Int Int
                | String String
                | List (List LogicValue)

type State = Map Int LogicValue

data SC = SC State Int

type Goal = SC → Stream SC

class AsLogicValue a where
  toLogicValue :: a → LogicValue

emptyState :: SC
emptyState = SC Map.empty 0

instance eqLogicValue :: Eq LogicValue where
  eq (LVar a) (LVar b) = a == b
  eq (Int a) (Int b) = a == b
  eq (String a) (String b) = a == b
  eq (List a) (List b) = a == b
  eq _ _ = false

instance showLogicValue :: Show LogicValue where
  show (LVar i) = "_." ++ show i
  show (Int i) = show i
  show (String s) = s
  show (List l) = let l' :: Array LogicValue
                      l' = List.toUnfoldable l
                  in show l'

instance showSC :: Show SC where
  show (SC s c) = "state=" ++ show s ++ ", counter=" ++ show c

instance logicValueAsLogicValue :: AsLogicValue LogicValue where
  toLogicValue = id

instance intAsLogicValue :: AsLogicValue Int where
  toLogicValue = Int

instance stringAsLogicValue :: AsLogicValue String where
  toLogicValue = String

instance foldableAsLogicValue :: (AsLogicValue a, Foldable f) ⇒ AsLogicValue (f a) where
  toLogicValue = List.fromFoldable >>> map toLogicValue >>> List

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
    unify' l r (Just s) = case Tuple (walk l s) (walk r s) of
      Tuple (LVar l) (LVar r) | l == r → Just s
      Tuple (LVar l) r → Just $ Map.insert l r s
      Tuple l (LVar r) → Just $ Map.insert r l s
      Tuple (List (Cons l ls)) (List (Cons r rs)) →
        unify' (List ls) (List rs) (unify' l r (Just s))
      Tuple l r | l == r → Just s
      _ → Nothing

infixl 4 equals as ?==

equals :: ∀ a b. (AsLogicValue a, AsLogicValue b) ⇒ a → b → Goal
equals l r = \(SC s c) → case unify (toLogicValue l) (toLogicValue r) s of
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
  List l → List $ map (flip walk_ s) l
  v → v

reify1st :: SC → LogicValue
reify1st (SC s _) = walk_ (LVar 0) s

run' :: Int → Goal → Array LogicValue
run' n = pull n >>> map reify1st >>> Lazy.toUnfoldable

run :: Goal → Array LogicValue
run g = []

infixl 0 run' as <?

appendo :: LogicValue → LogicValue → LogicValue → Goal
appendo l r out = (l ?== [] ?&& r ?== out)
                  ?|| (fresh3 \a d res → (conso a d ?== l) ?&& (conso a res ?== out) ?&& (appendo d r res))
