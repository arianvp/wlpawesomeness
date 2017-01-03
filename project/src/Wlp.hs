module Wlp where

import Language

-- import Data.List (foldl', foldr1)
import qualified Data.Set as Set (member)
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

-- TODO:
-- 1. unshadow   -- program paths can go through a var
-- 2. programpaths
-- 3. wlp
--
newtype Wlp = Wlp Expression

calcWlp :: [Statement] -> Expression -> Expression
calcWlp [] postc = postc
calcWlp (stmt:stmts) postc =
  case stmt of
    Skip -> calcWlp stmts postc
    Assert e -> e &&. calcWlp stmts postc
    Assume e -> e ==>. calcWlp stmts postc
    -- we _KNOW_ n if fresh due to preprocessing ,so
    -- this is safe
    Var _n _s -> error "calcWlp only supports cannonical statements, no foralls"
      -- foldr Forall (calcWlp s (calcWlp stmts postc)) n
    (n := e) -> substitute n e (calcWlp stmts postc)
    While _ _ -> error "calcWlp only supports cannonical statements, no branching"
    If _ _ _ -> error "calcWlp only supports cannonical statements, no branching"
