{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE TypeOperators #-}
module Substitute where
import Language
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Stack

fresh :: Name -> Set Name -> (Name, Name)
fresh x taken = fresh' x
  where
    fresh' y =
      if y `Set.member` taken
        then fresh' (y ++ "'")
        else (x, y)

substitute ::  HasCallStack => Expression -> Expression -> Expression -> Expression
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
    Forall n b -> Forall n (substitute name replaceBy b)
    Not e -> Not (substitute name replaceBy e)
    ArrayAt n i ->
      if ArrayAt n i == name
          then replaceBy
          else ArrayAt n i
