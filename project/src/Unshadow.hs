module Unshadow
( unshadow
) where

import Language
import Data.Map (Map)
import qualified Data.Map as Map

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

unshadowExpr :: Map Name Name -> Expression -> Expression
unshadowExpr names e =
  case e of
    IntVal x -> IntVal x
    BoolVal x -> BoolVal x
    BinOp b e1 e2 -> BinOp b (unshadowExpr names e1) (unshadowExpr names e2)
    Name n ->
      case Map.lookup n names of
        Nothing -> Name n
        Just n' -> Name n'
    Forall (Variable n t) e' ->
      -- make sure that n is renamed
      case Map.lookup n names of
        Nothing -> Forall (Variable n t) e'
        Just n' -> Forall (Variable n' t) (unshadowExpr names e')
    Not e' -> Not (unshadowExpr names e')
    ArrayAt n e' ->
      case Map.lookup n names of
        Nothing -> ArrayAt n (unshadowExpr names e')
        Just n' -> ArrayAt n' (unshadowExpr names e')


unshadow :: [Statement] -> [Statement]
unshadow = unshadowStmts Map.empty

unshadowStmts :: Map Name Name -> [Statement] -> [Statement]
unshadowStmts takenNames = unshadow'
  where
    unshadow' [] = []
    -- (x,x) (y,y)
    unshadow' (Var vars stmts:xs) =
      let findNewName :: Variable -> (Name, Name)
          findNewName (Variable name _typ) =
            fresh name (mappingToList takenNames)
          -- a map of renames
          renames :: Map Name Name
          renames = Map.fromList (map findNewName vars)
          setName (Variable name typ) =
            let newName = Map.lookup name renames
            in case newName of
                 Nothing -> Variable name typ
                 Just n -> Variable n typ
          renamedVars = map setName vars
          takenNames' = (Map.union renames takenNames)
          renamedStmts = unshadowStmts takenNames' stmts
      in Var renamedVars renamedStmts : unshadowStmts takenNames' xs
    unshadow' ((name := e):xs) =
      case Map.lookup name takenNames of
        Nothing -> (name := unshadowExpr takenNames e) : unshadow' xs
        Just newName -> ((newName := unshadowExpr takenNames e) : unshadow' xs)
    unshadow' (Skip:xs) = Skip : unshadow' xs
    unshadow' (Assert e:xs) = Assert (unshadowExpr takenNames e) : unshadow' xs
    unshadow' (Assume e:xs) = Assume (unshadowExpr takenNames e) : unshadow' xs
    unshadow' (If e s1 s2:xs) =
      If (unshadowExpr takenNames e) (unshadow' s1) (unshadow' s2) : unshadow' xs
    unshadow' (While e s:xs) =
      While (unshadowExpr takenNames e) (unshadow' s) : unshadow' xs
{-
program(x|y) {
  x := 1;        --  [(x,x),(y,y)]  []
  y := 3;
  var x in {  unshadowVar (variables + (x,x') }

    assert x'
    var y in {
      assert x < 4
    }
    assert x';
  }
  variables - [(x,x), (y,y)]
  x = 5;
}
-}
{-
weExpectYWithTwoPrimes =
  Program [Variable "x" (Prim Int)] [Variable "y" (Prim Int)]
   ["x" := IntVal 1
   ,"y" := IntVal 3
   , Var [Variable "y" (Prim Int)]
     [ Assert (Name "x")
     , Var [Variable "y" (Prim Int)]
       [ Assert (Name "y") ]
     ]
   ]

-- Var:y:xs
weExpectYWithTwoPrimesInBothCases =
  Program [Variable "x" (Prim Int)] [Variable "y" (Prim Int)]
   ["x" := IntVal 1
   ,"y" := IntVal 3
   , Var [Variable "y" (Prim Int)]  -- y'
     [ Assert (Name "x")
     , Var [Variable "y" (Prim Int)] -- y''
       [ Assert (Name "y") ]
     ]
   , "y" := IntVal 3 -- y
   , Var [Variable "y" (Prim Int)]   -- y'
     [ Assert (Name "x")
     , Var [Variable "y" (Prim Int)] -- y''
       [ Assert (Name "y") ]
     ]
   ,  "y" := IntVal 5 -- y  BUT NOT: y''''
   , Assert (Name "x" :=: IntVal 3)
   ]


lol =
  Program [Variable "x" (Prim Int)] [Variable "y" (Prim Int)]
  [ "x" := IntVal 1
  , Var [Variable "y" (Prim Int)]
    [ "y" := IntVal 1
    , Var [Variable "y" (Prim Int)]
      [ "y" := IntVal 3
      , "x" := (Name "x" :+: Name "y")
      , Assert (Name "x" :=: IntVal 4)
      ]
    ]
  , "y" := Name "x"
  , Var [Variable "y" (Prim Int)]
    [ "y" := IntVal 1
    , Var [Variable "y" (Prim Int)]
      [ "y" := IntVal 3
      , "x" := (Name "x" :+: Name "y")
      , Assert (Name "x" :=: IntVal 7)
      ]
    ]
  , Assert (Name "x" :=: IntVal 7)
  , "y" := Name "x"
  ]

-}
--
--e(x|y) {
--var y {
--}
--var y {
--}
--}
-- e(x|y) {
-- x := 1
-- assert x=1
-- var y' {
--  y := 1
--  assert y=1
--  var y {
--    y := 3
--    x := x + y
--    assert x=4
--  }
-- }
-- assert x = 4
-- y := x
-- }
--
