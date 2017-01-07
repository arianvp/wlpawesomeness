{-#LANGUAGE TupleSections #-}
module Property where
import Test.QuickCheck
import Data.Range.Range
import System.Random

import Language
import Substitute
import Eval


-- turns an arbitrary expression into a quickcheck property
-- based on a list of generators
prop :: [(Name, Gen Expression)] -> (Expression -> Property)
prop = foldr (accumulate . uncurry transformGen) (property . evalBool)
  where
    transformGen :: Name -> Gen Expression -> Gen (Name, Expression)
    -- pairs each generated expression with the name to substitute
    transformGen a = fmap (a, )
    accumulate
      :: Gen (Name, Expression)
      -> (Expression -> Property)
      -> (Expression -> Property)
    accumulate gen accum inExpr =
      forAll gen $ \(name, byExpr) -> accum $ substitute name byExpr inExpr


rangeToGen
  :: (Random a, Bounded a, Arbitrary a)
  => Range a -> Gen a
rangeToGen r =
  case r of
    SingletonRange x -> pure x
    SpanRange from to -> choose (from, to)
    LowerBoundRange lower -> choose (lower, maxBound)
    UpperBoundRange upper -> choose (minBound, upper)
    InfiniteRange -> arbitrary

rangesToGen
  :: (Random a, Bounded a, Arbitrary a)
  => [Range a] -> Gen a
rangesToGen = oneof . map rangeToGen


boolGen :: Gen Expression
boolGen = fmap BoolVal arbitrary

intGen :: Gen Expression
intGen = fmap IntVal arbitrary
