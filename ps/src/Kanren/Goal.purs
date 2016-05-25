module Kanren.Goal where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.Monoid (mempty)
import Data.Tuple (fst, Tuple(Tuple))
import Kanren.State (SC)
import Kanren.Stream (Stream)



newtype BS a = BS (SC → Stream (Tuple SC a))
type Goal = BS Unit

runGoal :: Goal → SC → Stream SC
runGoal (BS g) = g >>> map fst

instance bindBS :: Bind BS where
  bind (BS f) k = BS \sc → do
    (Tuple sc' a) ← f sc
    case k a of
      (BS g) -> g sc'

instance applyBS :: Apply BS where
  apply = ap

instance functorBS :: Functor BS where
  map = liftA1

instance applicativeBS :: Applicative BS where
  pure a = BS \sc → return $ Tuple sc a

instance monadBS :: Monad BS

instance altBS :: Alt BS where
  alt (BS f) (BS g) = BS \sc → f sc <> g sc

instance plusBS :: Plus BS where
  empty = BS (\sc → mempty)

instance alternativeBS :: Alternative BS

instance monadPlusBS :: MonadPlus BS
