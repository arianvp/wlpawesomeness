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
