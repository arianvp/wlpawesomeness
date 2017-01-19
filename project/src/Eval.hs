{-#LANGUAGE TupleSections #-}
module Eval where
import Language
import Substitute
import Free
import Test.SmallCheck hiding (forAll, exists)
import qualified Test.SmallCheck as Property
import Test.SmallCheck.Series


-- | Takes an expression in Sorted Prenex normal form,
-- and turns it into a smallCheck property
evalProp' :: Monad m => Expression -> Property m

-- first lets get rid of all primitive variables by substituting
-- them by values
evalProp' (Quantified (ForAll (Variable name (Prim Int))) expr) =
  let
    namedSeries :: Monad m => Series m (Name, Int)
    namedSeries =  fmap (name,) series
  in
    Property.over namedSeries $ \(_,x) ->
      evalProp' . reduce . substitute (Name name) (IntVal x) $ expr
evalProp' (Quantified (ForAll (Variable name (Prim Bool))) expr) =
  Property.forAll $ \x ->
    evalProp' . reduce . substitute (Name name) (BoolVal x) $ expr
evalProp' (Quantified (Exists (Variable name (Prim Int))) expr) =
  Property.forAll $ \x ->
    evalProp' . reduce . substitute (Name name) (IntVal x) $ expr
evalProp' (Quantified (Exists (Variable name (Prim Bool))) expr) =
  Property.exists $ \x ->
    evalProp' . reduce . substitute (Name name) (BoolVal x) $ expr

-- now we should only have array quantifiers left. At this point
-- arrays are the only free variables left, as all other free variables
-- have already been replaced by concrete values by smallCheck
-- we can now see each array index as a separate variable that we quantify over
evalProp' (Quantified (ForAll (Variable name (ArrayT (Array Int)))) expr) =
  -- to do this, lets first find all usages of the array that we're quantifying over.
  -- this are simply the free variables which have the same name as the
  -- current array
  let
    isCurrentArray (ArrayAt name' _) = name' == name
    isCurrentArray _ = False
    usages = filter isCurrentArray . free $ expr
    toQuant (ArrayAt name' e) accum =
      let
        repBy = name' ++ show e
      in
        forAll (Variable repBy (Prim Int)) (substitute (ArrayAt name' e) (Name repBy) accum)
    toQuant _ _ = error "should only have arrays at this point"
  in
    evalProp' $ foldr toQuant expr usages
evalProp' (Quantified (ForAll (Variable _name (ArrayT (Array Bool)))) _expr) =
  error "should be the same as integers. gotta implement later"
evalProp' (Quantified (Exists (Variable _name typ)) _expr) =
  error $ "We do not support existential quantification " ++ show typ

-- now we have removed all quantifiers, we know we do not have
-- any free variables left. we can proceed 
-- we handle implication specially, to short circuit if the
-- left hand is false. this way we avoid irrelevant test
-- cases on the right hand side
evalProp' (BinOp Impl a b) = evalProp' a ==> evalProp' b
-- other operators should simply be reduced
evalProp' (BinOp x a b) = evalProp' (reduce (BinOp x a b))
evalProp' (Not expr) = evalProp' (reduce (Not expr))
evalProp' (BoolVal x) = Property.forAll x
evalProp' (IntVal x) = error $ show x ++ " is not a boolean"
evalProp' (Name x) = error $ show x ++ " is still free"
evalProp' (ArrayAt x _) = error $ show x ++ " is still free"
evalProp' (IfThenElseE pred' a b) = evalProp' (reduce (IfThenElseE pred' a b))
evalProp' (ProgramCall _ _) =
  error "Program call should be gone at this point"


-- | Reduces an expression as far as possible
-- where as far as possible means:
-- * reduce until you hit a free variable
-- * Do not reduce implications, because those are magic in smallCheck
--
reduce :: Expression -> Expression
reduce (Name n) = Name n
reduce (ArrayAt name e) = ArrayAt name (reduce e)
reduce (BinOp Impl a b) =  reduce a :==>: reduce b
reduce (IntVal x) =  IntVal x
reduce (BoolVal x) =  BoolVal x
reduce (BinOp Plus a b) =
  case (reduce a, reduce b) of
    (IntVal a', IntVal b') ->
      IntVal (a' + b')
    (a', b') ->
      BinOp Plus a' b'
reduce (BinOp Min a b) =
  case (reduce a, reduce b) of
    (IntVal a', IntVal b') ->
      IntVal (a' - b')
    (a', b') ->
      BinOp Min a' b'
reduce (BinOp Conj a b) =
  case (reduce a, reduce b) of
    (BoolVal a', BoolVal b') ->
      BoolVal (a' && b')
    (a', b') ->
      BinOp Conj a' b'
reduce (BinOp Disj a b) =
  case (reduce a, reduce b) of
    (BoolVal a', BoolVal b') ->
      BoolVal (a' || b')
    (a', b') ->
      BinOp Disj a' b'
reduce (BinOp Le a b) =
  case (reduce a, reduce b) of
    (IntVal a', IntVal b') ->
      BoolVal (a' < b')
    (a', b') ->
      BinOp Le a' b'
reduce (BinOp Leq a b) =
  case (reduce a, reduce b) of
    (IntVal a', IntVal b') ->
      BoolVal (a' <= b')
    (a', b') ->
      BinOp Leq a' b'
reduce (BinOp Eq a b) =
  case (reduce a, reduce b) of
    (Name a', Name b') ->
      BoolVal (a' == b')
    (IntVal a', IntVal b') ->
      BoolVal (a' == b')
    (BoolVal a', BoolVal b') ->
      BoolVal (a' == b')
    (a', b') ->
      BinOp Eq a' b'
reduce (Quantified (ForAll names) e) = forAll names (reduce e)
reduce (Quantified (Exists names) e) = exists names (reduce e)
reduce (Not e) =
  case reduce e of
    BoolVal b -> BoolVal (not b)
    e' -> Not e'
reduce (IfThenElseE pred' a b) =
  case reduce pred' of
    BoolVal bl ->
      if bl then reduce a else reduce b
    e' -> IfThenElseE e' (reduce a) (reduce b)
reduce (ProgramCall _ _) =
  error "Program call should be gone at this point"






