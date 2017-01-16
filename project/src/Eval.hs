{-#LANGUAGE TupleSections #-}
module Eval where
import Language
import Substitute
import Free
import Test.SmallCheck hiding (forAll, exists)
import qualified Test.SmallCheck as Property
import Test.SmallCheck.Series

prop :: Monad m => [(Expression, Series m Expression)] -> (Expression -> Property m)
prop = foldr (accumulate . uncurry transformSeries) evalProp
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
      over series' $ \(name, byExpr) -> accum $ substitute name byExpr inExpr

isFullyEvaluated :: Expression -> Bool
isFullyEvaluated (BoolVal _) = True
isFullyEvaluated (IntVal _) = True
isFullyEvaluated _ = False

evalArrays :: Name -> Expression -> Expression
evalArrays name (ArrayAt name' e) =
  if name == name'
    then ArrayAt name' (eval' e)
    else ArrayAt name e
evalArrays name (BinOp x a b) =
  BinOp x (evalArrays name a) (evalArrays name b)
evalArrays name (Not e) =
  Not (evalArrays name e)
evalArrays _ (IntVal a) = IntVal a
evalArrays _ (BoolVal a) = BoolVal a
evalArrays _ (Name a) = Name a
evalArrays name (Quantified (ForAll y) e) = forAll y (evalArrays name e)
evalArrays _ (Quantified (Exists _) _) = error "dunno"



-- now that we know the expression is in prenex normal form,
-- we can start evaluating it!
-- It is important that we witness all the variables that can cause
-- an array index operation before we witness the array itself.
evalProp :: Monad m => Expression -> Property m
evalProp (BinOp Impl a b) = evalProp a ==> evalProp b
evalProp (BinOp x a b) = Property.forAll $ evalBool (BinOp x a b)
evalProp (Quantified (ForAll (Variable name typ)) a) =
  case typ of
    -- every time we encounter a variable, we should evaluate
    -- call sites as far as possible
    Prim Int ->
      let
        namedSeries :: Monad m => Series m (Name, Int)
        namedSeries =  fmap (name,) series
        f (_, x) = evalProp $ eval' $ substitute (Name name) (IntVal x) a
      in
        over namedSeries f
    Prim Bool -> error "todo"
    (ArrayT (Array typ')) ->
      -- euhm lets see if this works
      -- all that is left now, is quantify over the evaluated array
      let
        x = eval' a -- remove all array call sites
        toReplace = free x
        intSeries = fmap IntVal series
      in
        case typ' of
          Int ->
            -- we should now replace each instance of toReplace with a concrete value
            -- inside a
            -- important: for each we should use a separate generator, though they are
            -- the same type
            prop (zip toReplace (repeat intSeries)) x

          Bool -> error "idem dito"

        -- _evalArrays should eval the call sites, and requantify them

-- not Forall should be treated specially
{-evalProp (Not (Forall (Variable name typ) a)) =
  case typ of
    Prim Int ->
      let
        namedSeries :: Monad m => Series m (Name, Int)
        namedSeries =  fmap (name,) series
        f (_, x) = evalProp $ Not $ substitute (Name name) (IntVal x) a
      in
        Property.exists $ over namedSeries f
    Prim Bool ->
      let
        namedSeries :: Monad m => Series m (Name, Bool)
        namedSeries =  fmap (name,) series
        g :: (Name, Bool) -> Bool
        g (_, x) = evalBool $ Not $ substitute (Name name) (BoolVal x) a
      in
        Property.exists $ over namedSeries g
    _ -> error "array in ForAll not supported"
-}
evalProp (BoolVal x) = Property.forAll x
evalProp e = evalProp (eval' e)
--evalProp e = error $ "this is not a predicate : " ++ show e


evalBool :: Expression -> Bool
evalBool e =
  case eval' e of
    BoolVal b -> b
    e' -> error (show e')

-- | Evals an expression "as far as possible"
eval' :: Expression -> Expression
eval' (Name n) = Name n
eval' (IntVal x) =  IntVal x
eval' (BoolVal x) =  BoolVal x
eval' (BinOp Plus a b) =
  case (eval' a, eval' b) of
    (IntVal a', IntVal b') ->
      IntVal (a' + b')
    (a', b') ->
      BinOp Plus a' b'
eval' (BinOp Min a b) =
  case (eval' a, eval' b) of
    (IntVal a', IntVal b') ->
      IntVal (a' - b')
    (a', b') ->
      BinOp Min a' b'
eval' (BinOp Conj a b) =
  case (eval' a, eval' b) of
    (BoolVal a', BoolVal b') ->
      BoolVal (a' && b')
    (a', b') ->
      BinOp Conj a' b'
eval' (BinOp Disj a b) =
  case (eval' a, eval' b) of
    (BoolVal a', BoolVal b') ->
      BoolVal (a' || b')
    (a', b') ->
      BinOp Disj a' b'
eval' (BinOp Impl a b) =
  case (eval' a, eval' b) of
    (BoolVal a', BoolVal b') ->
      BoolVal (a' <= b')
    (a', b') ->
      BinOp Impl a' b'
eval' (BinOp Le a b) =
  case (eval' a, eval' b) of
    (IntVal a', IntVal b') ->
      BoolVal (a' < b')
    (a', b') ->
      BinOp Le a' b'
eval' (BinOp Leq a b) =
  case (eval' a, eval' b) of
    (IntVal a', IntVal b') ->
      BoolVal (a' <= b')
    (a', b') ->
      BinOp Leq a' b'
eval' (BinOp Eq a b) =
  case (eval' a, eval' b) of
    (IntVal a', IntVal b') ->
      BoolVal (a' == b')
    (BoolVal a', BoolVal b') ->
      BoolVal (a' == b')
    (a', b') ->
      BinOp Eq a' b'
eval' (Quantified (ForAll names) e) = forAll names (eval' e)
eval' (Quantified (Exists names) e) = exists names (eval' e)
eval' (Not e) =
  case eval' e of
    BoolVal e' -> BoolVal (not e')
    e' -> Not e'
eval' (ArrayAt name e) =
  ArrayAt name (eval' e)




