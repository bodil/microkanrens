module Kanren.Goal where

import Prelude
import Kanren.Stream (Stream)



newtype Goal a = Goal (a → Stream a)

runGoal :: ∀ a. Goal a → a → Stream a
runGoal (Goal g) = g
