{-#LANGUAGE TupleSections #-}
{-#LANGUAGE PatternSynonyms #-}
module Logic where
import Language
import Data.Range.Range
import Test.QuickCheck
import System.Random

-- strips away universal quantification at the beginning
strip :: Expression -> Expression
strip (Forall _ e) = strip e
strip e = e


normalize :: Expression -> Expression
-- the main rewrite rule
normalize (e1 :==>: (e2 :==>: e3)) =  normalize ((normalize (e1 :&&: e2)) :==>: normalize e3)

-- flatten booleans
normalize (BoolVal True :==>: e) = normalize e
normalize (BoolVal False :==>: _e) = BoolVal True
normalize (e1 :==>: e2) = normalize e1 :==>: normalize e2
normalize (e1 :&&: BoolVal True) = normalize e1
normalize (BoolVal True :&&: e1) = normalize e1
normalize (_e1 :&&: BoolVal False) = BoolVal False
normalize (e1 :&&: e2) = normalize e1 :&&: normalize e2
normalize (BoolVal False :&&: _e1) = BoolVal False
normalize (BoolVal False :||: e1) = normalize e1
normalize (e1 :||: BoolVal False) = normalize e1
normalize (BoolVal True :||: _e1) = BoolVal True
normalize (_e1 :||: BoolVal True) = BoolVal True


normalize (a :=: b) = normalize a :=: normalize b
normalize (a :<: b) = normalize a :<: normalize b
normalize (a :<=: b) = normalize a :<=: normalize b
normalize (Not a) = Not (normalize a)

-- right to left flattening of integer araithmatic
normalize ((e1 :-: (IntVal x)) :-: (IntVal y)) = normalize e1 :-: IntVal (x+y)
normalize (e1 :-: e2) = normalize e1 :-: normalize e2

normalize ((e1 :+: (IntVal x)) :+: (IntVal y)) = normalize e1 :+: IntVal (x+y)
normalize (e1 :+: e2) = normalize e1 :+: normalize e2

normalize e = e

-- given that we know how to generate free variables,
-- lets go and make a quickcheck property
--
--


{-prop :: Map Name (Gen Expression) -> Expression -> Property
prop gens e = foldlWithKey f _ gens
  where
    f :: Property -> Name -> Gen Expression -> Property
    f prop name gen =
      forAll gen $ \x -> prop .&&. (eval
-}

substitute :: Name -> Expression -> Expression -> Expression
substitute name replaceBy postc =
  case postc of
    Name x ->
      if x == name
        then replaceBy
        else Name x
    IntVal x -> IntVal x
    BoolVal x -> BoolVal x
    BinOp b x y ->
      BinOp b (substitute name replaceBy x) (substitute name replaceBy y)
    Forall n b -> Forall n (substitute name replaceBy b)
    Not e -> Not (substitute name replaceBy e)
    ArrayAt _ _ -> error "Arrays are not implemented yet. TODO"

-- todo: generalize this?
{-
 - Generalizes the following pattern:
prop :: [(Name, Gen Expression)] -> Expression -> Property
prop
  [ (a, aGen)
  , (b, bGen)
  , (c, cGen)
  , (d, dGen)
  ]
  expr
  =
  forAll aGen $ \a' ->
    forAll bGen $ \b' ->
      forAll cGen $ \c' ->
        forAll dGen $ \d' ->
          evalBool
          . substitute a a'
          . substitute b b'
          . substitute c c'
          . substitute d d'
          $ expr
-}


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

evalBool :: Expression -> Bool
evalBool e =
  let (BoolVal b) = eval e
  in b

  -- fuck that
eval :: Expression -> Expression
eval (Name _) = error "free variable"
eval (IntVal x) =  IntVal x
eval (BoolVal x) =  BoolVal x
eval (BinOp Plus a b) =
  let IntVal a' = eval a
      IntVal b' = eval b
  in IntVal (a' + b')
eval (BinOp Min a b) =
  let IntVal a' = eval a
      IntVal b' = eval b
  in IntVal (a' - b')
eval (BinOp Conj a b) =
  let BoolVal a' = eval a
      BoolVal b' = eval b
  in BoolVal (a' && b')
eval (BinOp Disj a b) =
  let BoolVal a' = eval a
      BoolVal b' = eval b
  in BoolVal (a' || b')
eval (BinOp Impl a b) =
  let BoolVal a' = eval a
      BoolVal b' = eval b
  in BoolVal (b' <= a')
eval (BinOp Le a b) =
  let IntVal a' = eval a
      IntVal b' = eval b
  in BoolVal (a' < b')
eval (BinOp Leq a b) =
  let IntVal a' = eval a
      IntVal b' = eval b
  in BoolVal (a' <= b')
eval (BinOp Eq a b) =
  case (eval a, eval b) of
    (BoolVal a', BoolVal b') ->
      BoolVal (a' == b')
    (IntVal a', IntVal b') ->
      BoolVal (a' == b')
    _ -> error "type error"
-- TODO this is a harder one, we want to try
-- several values of a. quickcheck
eval (Forall _ b) =
  eval b
eval (Not e) =
  let BoolVal e' = eval e
  in BoolVal (not e')
eval (ArrayAt _ _) = error "todo array"
