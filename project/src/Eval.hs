{-#LANGUAGE TupleSections #-}
module Eval where
import Language
import Substitute
import Test.SmallCheck
import Test.SmallCheck.Series

evalProp :: Monad m => Expression -> Property m
evalProp (BinOp Impl a b) = evalProp a ==> evalProp b
evalProp (BinOp x a b) = forAll (evalBool (BinOp x a b))
evalProp (Forall (Variable name typ) a) =
  case typ of
    Prim Int ->
      let
        namedSeries :: Monad m => Series m (Name, Int)
        namedSeries =  fmap (name,) series
        f :: (Name, Int) -> Bool
        f (_, x) = evalBool $ substitute name (IntVal x) a
      in
        over namedSeries f
    Prim Bool ->
      let
        namedSeries :: Monad m => Series m (Name, Bool)
        namedSeries =  fmap (name,) series
        g :: (Name, Bool) -> Bool
        g (_, x) = evalBool $ substitute name (BoolVal x) a
      in
        over namedSeries g
    _ -> error "array in ForAll not supported"
evalProp (Not _) = undefined
evalProp _ = error "this is not a predicate"

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
-- TODO this is a harder one, we want to try
-- several values of a. quickcheck
eval (Forall _ b) =
  eval b
eval (Not e) =
  let BoolVal e' = eval e
  in BoolVal (not e')
eval (ArrayAt _ _) = error "todo array"
