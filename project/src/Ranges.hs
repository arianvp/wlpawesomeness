-- | 'Ranges' modules takes a WLP and extracts a 'Goal' from it.
-- Once we have the 'Goal' we extract ranges from the 'Goal' assumption list
--
module Ranges where

import Language
import Data.Set (Set)

data Goal = Goal
  { assums :: Set Expression -- ^ the assumption list
  , concl :: Expression -- ^ the conclusion
  } deriving (Show)


toGoal :: Expression -> Expression
toGoal ((e1 :==>: (e2 :==>: e3))) = 
  


