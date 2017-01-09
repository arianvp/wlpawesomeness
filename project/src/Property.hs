{-#LANGUAGE TupleSections #-}
module Property where
import Test.SmallCheck
import Test.SmallCheck.Series

import Language
import Substitute
import Eval

prop :: Monad m => [(Name, Series m Expression)] -> (Expression -> Property m)
prop = foldr (accumulate . uncurry transformSeries) (evalProp)
  where
    transformSeries :: Name -> Series m Expression -> Series m (Name, Expression)
    transformSeries a = fmap (a,)
    accumulate
      :: Monad m
      => Series m (Name, Expression)
      -> (Expression -> Property m)
      -> (Expression -> Property m)
    accumulate series' accum inExpr =
      over series' $ \(name, byExpr) -> accum $ substitute (Name name) byExpr inExpr
