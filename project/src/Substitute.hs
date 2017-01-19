{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE TypeOperators #-}
module Substitute where
import Language
import qualified Data.Set as Set
import Data.Set (Set)

fresh :: Name -> Set Name -> (Name, Name)
fresh x taken = fresh' x
  where
    fresh' y =
      if y `Set.member` taken
        then fresh' (y ++ "'")
        else (x, y)

substituteArray :: AsgTarget -> Expression -> Expression -> Expression
substituteArray (N _) _ _ =
  error "only substitute arrays!"
substituteArray (A name index) replaceBy postc =
  case postc of
    ArrayAt n i ->
      if name == n
        then IfThenElseE (i :=: index) replaceBy (ArrayAt n i)
        else ArrayAt n (substituteArray (A name index) replaceBy i)
    IntVal x ->  IntVal x
    BoolVal x -> BoolVal x
    Name n -> Name n
    BinOp x a b ->
      BinOp x
        (substituteArray (A name index) replaceBy a)
        (substituteArray (A name index) replaceBy b)
    Not e -> Not (substituteArray (A name index) replaceBy e)
    Quantified (ForAll n) e -> forAll n (substituteArray (A name index) replaceBy e)
    Quantified (Exists n) e -> exists n (substituteArray (A name index) replaceBy e)
    ProgramCall globalName exprs -> ProgramCall globalName (map (substituteArray (A name index) replaceBy) exprs)
    IfThenElseE a b c ->
      IfThenElseE
        (substituteArray (A name index) replaceBy a)
        (substituteArray (A name index) replaceBy b)
        (substituteArray (A name index) replaceBy c)

substitute ::  Expression -> Expression -> Expression -> Expression
substitute name replaceBy postc =
  case postc of
    Name x ->
      if Name x == name
        then replaceBy
        else Name x
    IntVal x -> IntVal x
    BoolVal x -> BoolVal x
    BinOp b x y ->
      BinOp b (substitute name replaceBy x) (substitute name replaceBy y)
    Quantified (ForAll n) b -> forAll n (substitute name replaceBy b)
    Quantified (Exists n) b -> exists n (substitute name replaceBy b)
    Not e -> Not (substitute name replaceBy e)
    ProgramCall globalName exprs -> ProgramCall globalName (map (substitute name replaceBy) exprs)
    IfThenElseE pred' a b ->
      IfThenElseE
        (substitute name replaceBy pred')
        (substitute name replaceBy a)
        (substitute name replaceBy b)
    ArrayAt n i ->
      if ArrayAt n i == name
          then replaceBy
          else ArrayAt n (substitute name replaceBy i)
