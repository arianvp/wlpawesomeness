{-#LANGUAGE TupleSections #-}
{-#LANGUAGE PatternSynonyms #-}
module Logic where
import Language

-- strips away universal quantification at the beginning
strip :: Expression -> Expression
strip (Quantified (ForAll _) e) = strip e
strip (Quantified (Exists _) e) = strip e
strip e = e

strip' :: Expression -> (Expression, [Quantifier])
strip' (Quantified q e) =
  let (e', quants) = strip' e
  in (e', q:quants)
strip' e = (e, [])

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
normalize (Quantified (ForAll t) e) = forAll t (normalize e)
normalize e = e

-- move all the quantors to the front
prenex :: Expression -> Expression
prenex e =
  if prenex' e == e
    then e
    else prenex (prenex' e)
prenex' :: Expression -> Expression
prenex' (Quantified (ForAll x) e) = forAll x (prenex' e)
prenex' (Quantified (Exists x) e) = exists x (prenex' e)
prenex' (Not (Quantified (Exists x) a)) = forAll x (Not a)
prenex' (Not (Quantified (ForAll x) a)) = exists x (Not a)
prenex' (a :==>: Quantified (Exists x) b) = exists x (a :==>: b)
prenex' (a :==>: Quantified (ForAll x) b) = forAll x (a :==>: b)
prenex' (Quantified (ForAll x) a :==>: b) = exists x (a :==>: b)
prenex' (Quantified (Exists x) a :==>: b) = forAll x (a :==>: b)
prenex' (Quantified (Exists x) a :||: b) = exists x (a :||: b)
prenex' (a :||: Quantified (Exists x) b) = exists x (a :||: b)
prenex' (Quantified (Exists x) a :&&: b) = exists x (a :&&: b)
prenex' (a :&&: Quantified (Exists x) b) = exists x (a :&&: b)
prenex' (Quantified (ForAll x) a :||: b) = forAll x (a :||: b)
prenex' (a :||: Quantified (ForAll x) b) = forAll x (a :||: b)
prenex' (Quantified (ForAll x) a :&&: b) = forAll x (a :&&: b)
prenex' (a :&&: Quantified (ForAll x) b) = forAll x (a :&&: b)
prenex' (a :==>: b) = prenex' a :==>: prenex' b
prenex' (a :&&: b) = prenex' a :&&: prenex' b
prenex' (a :||: b) = prenex' a :||: prenex' b
prenex' a = a


