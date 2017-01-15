{-#LANGUAGE TupleSections #-}
module Eval where
import Language
import Substitute
import Test.SmallCheck
import Test.SmallCheck.Series
import GHC.Stack

evalProp :: Monad m => Expression -> Property m
evalProp (BinOp Impl a b) = evalProp a ==> evalProp b
evalProp (BinOp x a b) = forAll (evalBool (BinOp x a b))
evalProp (Forall (Variable name typ) a) =
  case typ of
    Prim Int ->
      let
        namedSeries :: Monad m => Series m (Name, Int)
        namedSeries =  fmap (name,) series
        f (_, x) = evalProp $ substitute (Name name) (IntVal x) a
      in
        over namedSeries f
    Prim Bool ->
      let
        namedSeries :: Monad m => Series m (Name, Bool)
        namedSeries =  fmap (name,) series
        g (_, x) = evalProp $ substitute (Name name) (BoolVal x) a
      in
        over namedSeries g
    _ -> error "array in ForAll not supported"

-- not Forall should be treated specially
evalProp (Not (Forall (Variable name typ) a)) =
  case typ of
    Prim Int ->
      let
        namedSeries :: Monad m => Series m (Name, Int)
        namedSeries =  fmap (name,) series
        f (_, x) = evalProp $ Not $ substitute (Name name) (IntVal x) a
      in
        exists $ over namedSeries f
    Prim Bool ->
      let
        namedSeries :: Monad m => Series m (Name, Bool)
        namedSeries =  fmap (name,) series
        g (_, x) = evalProp $ Not $ substitute (Name name) (BoolVal x) a
      in
        exists $ over namedSeries g
    _ -> error "array in ForAll not supported"
evalProp (Not e) = forAll $ evalBool (Not e)
evalProp (BoolVal x) = forAll x
evalProp _ = error "this is not a predicate"


evalBool :: HasCallStack => Expression -> Bool
evalBool e =
  let (BoolVal b) = eval e
  in b

eval :: HasCallStack => Expression -> Expression
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
  in BoolVal (a' <= b')
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
eval (Forall _ _b) =
  error "should never occur"
eval (Not e) =
  let BoolVal e' = eval e
  in BoolVal (not e')
eval (ArrayAt name e) = ArrayAt name (eval e)
