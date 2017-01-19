module Unshadow
( unshadow
) where

import Language
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

{-variableToName :: Variable -> (Name, Name)
variableToName (Variable name _) = (name, name)-}

{-unshadowProgram :: Program -> Program
unshadowProgram (Program inputs outputs statements) =
  let inputNames = map variableToName inputs
      outputNames = map variableToName outputs
      names = (Map.fromList (inputNames ++ outputNames))
      newStatements = unshadowStmts names statements
  in Program inputs outputs newStatement
-}

{-
 - e(x|y) {
 - x + y
 -}
-- returns a mapping from old name to new name
fresh :: Name -> [Name] -> (Name, Name)
fresh x taken = fresh' x
  where
    fresh' y =
      if y `elem` taken
        then fresh' (y ++ "'")
        else (x, y)

mappingToList :: Map Name Name -> [Name]
mappingToList m = Map.elems m ++ Map.keys m


unshadowExpr :: Expression -> State (Map Name Name) Expression
unshadowExpr (IntVal x) = pure (IntVal x)
unshadowExpr (BoolVal x) = pure (BoolVal x)
unshadowExpr (BinOp op e1 e2) = do
  e1' <- unshadowExpr e1
  e2' <- unshadowExpr e2
  pure (BinOp op e1' e2')
unshadowExpr (Name n) = do
  names <- get
  case Map.lookup n names of
    Nothing -> pure (Name n)
    Just n' -> pure (Name n')
unshadowExpr (Quantified (ForAll var) e) = do
  var' <- unshadowVariable var
  e' <- unshadowExpr e
  pure (forAll var' e')
unshadowExpr (Quantified (Exists var) e) = do
  var' <- unshadowVariable var
  e' <- unshadowExpr e
  pure (exists var' e')
unshadowExpr (Not e) = do
  e' <- unshadowExpr e
  pure (Not e')
unshadowExpr (ProgramCall globalName es) = do
  es' <- mapM unshadowExpr es
  pure (ProgramCall globalName es')
unshadowExpr (ArrayAt n e) = do
  names <- get
  e' <- unshadowExpr e
  case Map.lookup n names of
    Nothing -> pure (ArrayAt n e')
    Just n' -> pure (ArrayAt n' e')
unshadowExpr (IfThenElseE pred' a b) = do
  pred'' <- unshadowExpr pred'
  a' <- unshadowExpr a
  b' <- unshadowExpr b
  pure (IfThenElseE pred'' a' b')


unshadow :: [Statement] -> [Statement]
unshadow xs = evalState (unshadow' xs) Map.empty


unshadowVariable :: Variable -> State (Map Name Name) Variable
unshadowVariable (Variable name typ) = do
  names <- get
  let (_, newName) = fresh name (mappingToList names)
  put (Map.insert name newName names)
  pure (Variable newName typ)

unshadow' :: [Statement] -> State (Map Name Name) [Statement]
unshadow' [] = pure []
unshadow' (Var vars stmts:xs) = do
  vars' <- mapM unshadowVariable vars
  stmts' <- unshadow' stmts
  xs' <- unshadow' xs
  pure (Var vars' stmts':xs')
unshadow' ((N name := e):xs) = do
  names <- get
  e' <- unshadowExpr e
  xs' <- unshadow' xs
  case Map.lookup name names of
    Nothing ->
      pure ((N name := e'):xs')
    Just name' ->
      pure ((N name' := e'):xs')
unshadow' ((A name index := e):xs) = do
  names <- get
  index' <- unshadowExpr index
  e' <- unshadowExpr e
  xs' <- unshadow' xs
  case Map.lookup name names of
    Nothing ->
      pure ((A name index' := e'):xs')
    Just name' ->
      pure ((A name' index' := e'):xs')
unshadow' (Skip:xs) = do
  xs' <- unshadow' xs
  pure (Skip:xs')
unshadow' (Assert e:xs) = do
  e' <- unshadowExpr e
  xs' <- unshadow' xs
  pure (Assert e' : xs')
unshadow' (Assume e:xs) = do
  e' <- unshadowExpr e
  xs' <- unshadow' xs
  pure (Assume e' : xs')
unshadow' (If e s1 s2:xs) = do
  e' <- unshadowExpr e
  s1' <- unshadow' s1
  s2' <- unshadow' s2
  xs' <- unshadow' xs
  pure (If e' s1' s2': xs')

unshadow' (While e s1:xs) = do
  e' <- unshadowExpr e
  s1' <- unshadow' s1
  xs' <- unshadow' xs
  pure (While e' s1': xs')
