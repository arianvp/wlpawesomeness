module Unshadow where

import Language
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

variableToName :: Variable -> (Name,Name)
variableToName (Variable name typ) = (name,name)

unshadowProgram :: Program -> Program
unshadowProgram (Program inputs outputs statements) =
  let
    inputNames = map variableToName inputs
    outputNames = map variableToName outputs
    names  = (Map.fromList (inputNames ++ outputNames))
    newStatements = unshadowVar names statements
  in
    Program inputs outputs newStatements


{-
 - e(x|y) {
 - x + y
 -}

-- returns a mapping from old name to new name
fresh :: Name -> [Name] -> (Name, Name)
fresh x taken =
  fresh' x
  where
    fresh' y =
      if y `elem` taken
        then fresh' (y++"'")
        else (x, y)

mappingToList :: Map Name Name -> [Name]
mappingToList map = Map.elems map ++ Map.keys map

unshadowExpr :: Map Name Name -> Expression -> Expression
unshadowExpr names (IntVal x) = IntVal x
unshadowExpr names (BinOp b e1 e2) =  BinOp b (unshadowExpr names e1) (unshadowExpr names e2)
unshadowExpr names (Name n) =
  case Map.lookup n names of
    Nothing -> Name n
    Just n' -> Name n'

unshadowVar :: Map Name Name -> [Statement] -> [Statement]
unshadowVar takenNames = unshadow'
  where
    unshadow' [] = []
    -- (x,x) (y,y)
    unshadow' (Var vars stmts:xs) =
      let
        findNewName :: Variable -> (Name, Name)
        findNewName (Variable name typ) =
          fresh name (mappingToList takenNames)
        -- a map of renames
        renames :: Map Name Name
        renames = Map.fromList (map findNewName vars)
        setName (Variable name typ) =
          let
            newName = Map.lookup name renames
          in
            case newName of
              Nothing -> Variable name typ
              Just n -> Variable n typ
        renamedVars = map setName vars
        -- fromList [("x","x"),("y","y'")]
        lol = (Map.union renames takenNames)
        renamedStmts = unshadowVar lol stmts
      in
        (Var renamedVars renamedStmts) : unshadow' xs

    unshadow' ((name := e):xs) =
      case Map.lookup name takenNames of
        Nothing -> ((name := unshadowExpr takenNames e):unshadow' xs)
        Just newName ->
          ((newName := unshadowExpr takenNames e):unshadow' xs)
    unshadow' (Skip:xs) = Skip:unshadow' xs
    unshadow' (Assert e:xs) = Assert (unshadowExpr takenNames e) : unshadow' xs
    unshadow' (Assume e:xs) = Assume (unshadowExpr takenNames e) : unshadow' xs

{-unshadowStmts :: [Name] -> [Statement] -> [Statement]
unshadowStmts names [] = []
-- we only have to rename if we encounter a var
unshadowStmts names (Var vars' stmts:xs) =
  let
    names' = map variableToName vars'
  in
    Var vars' (unshadowVar names' stmts) : unshadowStmts names xs
unshadowStmts names (a:xs) = a:unshadowStmts names xs
unshadowStmts names (Skip:xs) =
  Skip : unshadowStmts names xs
unshadowStmts names (Assert e:xs) =
  Assert (unshadowExpr names e):unshadowStmts names xs
unshadowStmts names (Assume e:xs) =
  Assume (unshadowExpr names e):unshadowStmts names xs
unshadowStmts names ((name := e):xs)

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

