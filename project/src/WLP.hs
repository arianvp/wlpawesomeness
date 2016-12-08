module WLP where

import Language.Canonical

import Data.Monoid
import Control.Applicative
import Control.Monad.State (State)
import Control.Monad.State.Class
import Data.List (foldl', foldr1)
import qualified Data.Set as Set (fromList, singleton, member)
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import Data.Map (Map, (!))


-- calculates the free variables in a list of statements
freeVars :: [Statement] -> Set Name
freeVars [] = mempty
freeVars (x:xs) =
  case x of
    Skip -> freeVars xs
    Assert e ->
      freeVars' e <> freeVars xs
    Assume e ->
      freeVars' e <> freeVars xs
    (n := e) ->
      freeVars' e <> freeVars xs
    Var vs ys ->
      freeVars ys \\ Set.fromList (map (\(Variable name _) -> name) vs)


-- calculates free variables inside an expression
freeVars' :: Expression -> Set Name
freeVars' (Name i) = Set.singleton i
freeVars' (e1 :+: e2) = freeVars' e1 <> freeVars' e2
freeVars' (e1 :-: e2) = freeVars' e1 <> freeVars' e2
freeVars' (e1 :&&: e2) = freeVars' e1 <> freeVars' e2
freeVars' (e1 :||: e2) = freeVars' e1 <> freeVars' e2
freeVars' (e1 :=>: e2) = freeVars' e1 <> freeVars' e2
freeVars' (e1 :<: e2) = freeVars' e1 <> freeVars' e2
freeVars' (e1 :<=: e2) = freeVars' e1 <> freeVars' e2
freeVars' (e1 :=: e2) = freeVars' e1 <> freeVars' e2
freeVars' (Forall (Variable i typ) e) = freeVars' e \\ Set.singleton i


substitute :: Name -> Expression -> Expression -> Set Name -> Expression
substitute v x b boundVariables =
  sub b
  where
    -- we can always assume that the names in the
    -- expression x are always sensible and thus bound
    -- if it is not bound then don't care either.
    -- basically, we do not care
    sub (Name i) =
      if v == i
        then x
        else Name i
    sub (e1 :+: e2) = sub e1 :+: sub e2
    sub (e1 :-: e2) = sub e1 :-: sub e2
    sub (e1 :&&: e2) = sub e1 :&&: sub e2
    sub (e1 :||: e2) = sub e1 :||: sub e2
    sub (e1 :=>: e2) = sub e1 :=>: sub e2
    sub (e1 :<: e2) = sub e1 :<: sub e2
    sub (e1 :<=: e2) = sub e1 :<=: sub e2
    sub (e1 :=: e2) = sub e1 :=: sub e2

    sub (Forall (Variable i typ) e) =
      if v == i
        -- if the target name v we want to replace
        -- is the same name as that Forall quantifies over
        -- then we should not continue the substitution right?
        -- we are simply done
      then  Forall (Variable i typ) e

      -- if the name that Forall quantifies over
      -- is in the bound variables, we should
      -- give it a new name. Also, if it is free,
      -- we should give it a new nice name.
      else if Set.member i (fvx <> boundVariables)
      then
        let i' = cloneSym e i
            e' = substitute i (Name i') e boundVariables
        in Forall (Variable i' typ) (sub e')
      else
          Forall (Variable i typ) e
    fvx = freeVars' x
    cloneSym e i = loop i
      where
        loop (i') =
          if Set.member (i') vars
            then loop ((i' ++ "'"))
            else i'
        vars = fvx <> freeVars' e



-- unshadow a x (
-- var x, y in {
--    x = a
--    y = b
--    x = x + y
--    x = (forall x. x)
--    var x in {
--      x = y + x
--    }
-- })
-- =>
-- var x', y in {
--   x' = x
--   x' = x' + y
--   x' = (forall x . x )
-- }
--
{- substStmt
  :: [Variable]   -- all the variables defined by var
  -> [Statement]  -- inside statement
  -> [Name]       -- should not conflict with these names.
  -> ([Statement]  -- we return a version where the conflicts are resolved.
-}


fresh :: Name -> Set Name -> (Name, Name)
fresh x taken =
  fresh' x
  where
    fresh' y =
      if y `Set.member` taken
        then fresh' (y++"'")
        else (x, y)

unshadow' :: Expression -> Map Name Name -> Expression
unshadow' (IntVal i) k = IntVal i
unshadow' (BoolVal b) k = BoolVal b
unshadow' (Name str) k = Name (maybe str id (Map.lookup str k))
unshadow' (a :+: b) k =  unshadow' a k :+: unshadow' b k
unshadow' (a :-: b) k =  unshadow' a k :-: unshadow' b k
unshadow' (a :&&: b) k =  unshadow' a k :&&: unshadow' b k
unshadow' (a :||: b) k =  unshadow' a k :||: unshadow' b k
unshadow' (a :=>: b) k =  unshadow' a k :=>: unshadow' b k
unshadow' (a :<: b) k =  unshadow' a k :<: unshadow' b k
unshadow' (a :<=: b) k =  unshadow' a k :<=: unshadow' b k
unshadow' (a :=: b) k =  unshadow' a k :=: unshadow' b k
unshadow' (Not e) k = Not (unshadow' e k)
unshadow' (Forall (Variable n t) e) k =
  let renames = uncurry Map.insert (fresh n (Map.keysSet k)) k
  in Forall (Variable (maybe n id (Map.lookup n renames)) t) (unshadow' e renames)

-- var x in {
--  x = a
-- };
-- var x, y in {
--    x = a
--    y = b
--    x = x + y
--    x = (forall x. x)
--    var x in {
--      x = y + x
--    }
-- })
--

unshadow :: [Statement] -> Map Name Name -> [Statement]
unshadow [] names = []
unshadow (Skip:xs) names = Skip:unshadow xs names
unshadow (Assert e:xs) names = Assert (unshadow' e names):unshadow xs names
unshadow (Assume e:xs) names = Assume (unshadow' e names):unshadow xs names
unshadow (Var names' body:xs) names =
  -- renames is a map of renames by choosing new names for variables introduced
  -- by var
  let
    renames = Map.fromList (map (\(Variable n t) -> fresh n (Map.keysSet names)) names') <> names
  in -- now we actually rename them?
    Var (map (\(Variable n t) -> Variable (maybe n id (Map.lookup n renames)) t) names') (unshadow body renames) : unshadow xs renames


calcWlp :: [Statement] -> Expression -> Expression
calcWlp [] postc = postc
calcWlp (stmt:stmts) postc =
  case stmt of
    Skip -> calcWlp stmts postc
    Assert e -> e :&&: calcWlp stmts postc
    Assume e -> e :=>: calcWlp stmts postc
    -- we have preprocessed such that no bound variable is shadowed
    Var n s ->  calcWlp s (calcWlp stmts postc)
    (n := e) -> substitute n e (calcWlp stmts postc) mempty
