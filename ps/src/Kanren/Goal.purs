module Kanren.Goal where

import Prelude
import Kanren.State (SC)
import Kanren.Stream (Stream)

type Goal = SC → Stream SC
