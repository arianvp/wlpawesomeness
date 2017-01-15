{-#LANGUAGE TupleSections #-}
module Property where
import Test.SmallCheck
import Test.SmallCheck.Series

import Language
import Substitute
import Eval

import Debug.Trace 

prop :: Monad m => [(Expression, Series m Expression)] -> (Expression -> Property m)
prop = foldr (accumulate . uncurry transformSeries) (evalProp . traceShowId)
  where
    transformSeries :: Expression -> Series m Expression -> Series m (Expression, Expression)
    transformSeries a = fmap (a,)
    accumulate
      :: Monad m
      => Series m (Expression, Expression)
      -> (Expression -> Property m)
      -> (Expression -> Property m)
    accumulate series' accum inExpr =
      -- TODO before we substitute an array, we should fully evaluate it's indexing operator
      -- we can then replace its entire site with an expression
      let
      in
        over series' $ \(name, byExpr) -> accum $ substitute name byExpr inExpr
