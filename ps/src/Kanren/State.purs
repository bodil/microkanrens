module Kanren.State where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))

import Kanren.Value (LogicValue(LVar, Pair))



type State = Map Int LogicValue

data SC = SC State Int



emptyState :: SC
emptyState = SC Map.empty 0

instance showSC :: Show SC where
  show (SC s c) = "state=" ++ show s ++ ", counter=" ++ show c



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
