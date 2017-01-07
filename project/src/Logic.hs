{-#LANGUAGE PatternSynonyms #-}
module Logic where
import Language
--import Data.Range.Range
import Data.Map

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

eval :: Map Name Expression -> Expression -> Expression
eval vals (Name x) = vals ! x
eval _ (IntVal x) =  IntVal x
eval _ (BoolVal x) =  BoolVal x
eval n (BinOp Plus a b) =
  let IntVal a' = eval n a
      IntVal b' = eval n b
  in IntVal (a' + b')
eval n (BinOp Min a b) =
  let IntVal a' = eval n a
      IntVal b' = eval n b
  in IntVal (a' - b')
eval n (BinOp Conj a b) =
  let BoolVal a' = eval n a
      BoolVal b' = eval n b
  in BoolVal (a' && b')
eval n (BinOp Disj a b) =
  let BoolVal a' = eval n a
      BoolVal b' = eval n b
  in BoolVal (a' || b')
eval n (BinOp Impl a b) =
  let BoolVal a' = eval n a
      BoolVal b' = eval n b
  in BoolVal (b' <= a')
eval n (BinOp Le a b) =
  let IntVal a' = eval n a
      IntVal b' = eval n b
  in BoolVal (b' < a')
eval n (BinOp Leq a b) =
  let IntVal a' = eval n a
      IntVal b' = eval n b
  in BoolVal (b' <= a')
eval n (BinOp Eq a b) =
  case (eval n a, eval n b) of
    (BoolVal a', BoolVal b') ->
      BoolVal (a' == b')
    (IntVal a', IntVal b') ->
      BoolVal (a' == b')
    _ -> error "type error"
-- TODO this is a harder one, we want to try
-- several values of a. quickcheck
eval n (Forall _ b) =
  eval n b
eval n (Not e) =
  let BoolVal e' = eval n e
  in BoolVal (not e')
eval _ (ArrayAt _ _) = error "todo array"
