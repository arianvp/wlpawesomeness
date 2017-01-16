{-#LANGUAGE TupleSections #-}
{-#LANGUAGE PatternSynonyms #-}
module Logic where
import Language

-- strips away universal quantification at the beginning
strip :: Expression -> Expression
strip (Forall _ e) = strip e
strip (Exists _ e) = strip e
strip e = e


normalize :: Expression -> Expression
-- the main rewrite rule to remove false postives
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
normalize (Forall t e) = Forall t (normalize e)
normalize e = e

-- move all the quantors to the front
prenex :: Expression -> Expression
prenex e =
  if prenex' e == e
    then e
    else prenex (prenex' e)
prenex' :: Expression -> Expression
prenex' (Forall x e) = Forall x (prenex' e)
prenex' (Exists x e) = Exists x (prenex' e)
prenex' (Not (Exists x a)) = Forall x (Not a)
prenex' (Not (Forall x a)) = Exists x (Not a)
prenex' (a :==>: Exists x b) = Exists x (a :==>: b)
prenex' (a :==>: Forall x b) = Forall x (a :==>: b)
prenex' (Forall x a :==>: b) = Exists x (a :==>: b)
prenex' (Exists x a :==>: b) = Forall x (a :==>: b)
prenex' (Exists x a :||: b) = Exists x (a :||: b)
prenex' (a :||: Exists x b) = Exists x (a :||: b)
prenex' (Exists x a :&&: b) = Exists x (a :&&: b)
prenex' (a :&&: Exists x b) = Exists x (a :&&: b)
prenex' (Forall x a :||: b) = Forall x (a :||: b)
prenex' (a :||: Forall x b) = Forall x (a :||: b)
prenex' (Forall x a :&&: b) = Forall x (a :&&: b)
prenex' (a :&&: Forall x b) = Forall x (a :&&: b)
prenex' (a :==>: b) = prenex' a :==>: prenex' b
prenex' (a :&&: b) = prenex' a :&&: prenex' b
prenex' (a :||: b) = prenex' a :||: prenex' b
prenex' a = a


