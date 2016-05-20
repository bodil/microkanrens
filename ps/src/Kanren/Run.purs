module Kanren.Run where

import Prelude
import Data.List.Lazy as Lazy
import Kanren.Goal (Goal)
import Kanren.State (walk, emptyState, State, SC(SC))
import Kanren.Stream (Stream)
import Kanren.Value (LogicValue(Pair, LVar))



callGoal :: Goal → Stream SC
callGoal g = g emptyState

pull :: Int → Goal → Stream SC
pull n = callGoal >>> Lazy.take n

walk_ :: LogicValue → State → LogicValue
walk_ v s = case walk v s of
  Pair car cdr → Pair (walk_ car s) (walk_ cdr s)
  v → v

reify1st :: SC → LogicValue
reify1st (SC s _) = walk_ (LVar 0) s

run' :: Int → Goal → Array LogicValue
run' n = pull n >>> map reify1st >>> Lazy.toUnfoldable

run :: Goal → Array LogicValue
run = callGoal >>> map reify1st >>> Lazy.toUnfoldable

infixl 0 run' as <?
