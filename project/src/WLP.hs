module WLP where

import Language.Canonical

import Data.Monoid
import Control.Applicative
import Control.Monad.State (State)
import Control.Monad.State.Class
import Data.List (foldl')
import qualified Data.Set as Set
import Data.Set (Set)

-- Some notes:
-- input and output variables are encoded as implicitly bound variables.
-- assigning to or reading from an unbound variable is an error and is considered an invalid
-- program


freeVars :: Expression -> Set Name
freeVars (Name i) = Set.singleton i
freeVars (e1 :+: e2) = freeVars e1 <> freeVars e2
freeVars (e1 :-: e2) = freeVars e1 <> freeVars e2
freeVars (e1 :&&: e2) = freeVars e1 <> freeVars e2
freeVars (e1 :||: e2) = freeVars e1 <> freeVars e2
freeVars (e1 :=>: e2) = freeVars e1 <> freeVars e2
freeVars (e1 :<: e2) = freeVars e1 <> freeVars e2
freeVars (e1 :<=: e2) = freeVars e1 <> freeVars e2
freeVars (e1 :=: e2) = freeVars e1 <> freeVars e2
freeVars (Forall (Variable i typ) e) =
  Set.difference (freeVars e) (Set.singleton i)

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
    fvx = freeVars x
    cloneSym e i = loop i
      where
        loop (i') =
          if Set.member (i') vars
            then loop ((i' ++ "'"))
            else i'
        vars = fvx <> freeVars e

calcWlp :: [Statement] -> Expression -> Expression
calcWlp [] postc = postc
calcWlp (stmt:stmts) postc =
  case stmt of
    Skip -> calcWlp stmts postc
    Assert e -> e :&&: calcWlp stmts postc
    Assume e -> e :=>: calcWlp stmts postc
    (n := e) -> substitute n e (calcWlp stmts postc) mempty
