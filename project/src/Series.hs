module Series where

import Test.SmallCheck.Series
import Language

ints :: Monad m => Series m Expression
ints = fmap IntVal series

bools :: Monad m => Series m Expression
bools = fmap BoolVal series
