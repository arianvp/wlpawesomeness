module ProgramCall where

import Data.Maybe
import Language
import Programs
import Control.Monad.State
import Data.Map (Map)
import Unshadow
import qualified Data.Map as Map

type LUT = [(Name, [Statement])]

lookupTable :: LUT
lookupTable =
  [ ("minind", minind)
  , ("swap", swap)
  , ("example1", example1)
  , ("example2", example2)
  ]

example1 :: [Statement]
example1 =
  [ Var
      [ Variable "k" (Prim Int)
      , Variable "m" (Prim Int)
      , Variable "r" (Prim Int)
      ]
      [N "k" := (Name "k" :+: IntVal 1), N "r" := (Name "k" :+: Name "m")]
  ]

example2 :: [Statement]
example2 =
  [ Var
      [ Variable "c" (Prim Int)
      , Variable "p" (Prim Int)
      , Variable "d" (Prim Int)
      ]
      [ N "c" := IntVal 3
      , N "p" := IntVal 1
      , N "d" := ProgramCall "example1" [Name "c" :+: IntVal 1, Name "p"]
      , N "p" := Name "d"
      ]
  ]

assignInputs :: String -> Expression -> Statement
assignInputs s1 s2 = N s1 := s2

stringFromVariable :: Variable -> String
stringFromVariable (Variable n _) = n

--finds ProgramCall and replace it using replacePC function, already used variables stored in usedVar
transform1 :: [Statement] -> [Variable] -> [Statement]
transform1 (Var a b:stmts) usedVar =
  Var a (transform1 b (usedVar ++ a)) : transform1 stmts (usedVar ++ a)
transform1 (tag := ProgramCall pr expr:stmts) usedVar =
  replacePC tag pr expr usedVar ++ transform1 stmts usedVar
transform1 (stmt:stmts) usedVar = stmt : transform1 stmts usedVar
transform1 [] _usedVar = []

replacePC :: AsgTarget -> Name -> [Expression] -> [Variable] -> [Statement]
replacePC (N output1) progName inputs1 _usedVar =
  [Var variables (buildNewstatements variables inputs1 output1 statements)]
  where
    (Var variables statements:_stmts) = fromJust (lookup progName lookupTable)
replacePC (A _ _) _progName _inputs1 _usedVar = error "todo"

-- TODO:: this are statements of program we are calling, we should first unshadow this using usedVar
--newstatements is [assign input, body, assign output]
buildNewstatements :: [Variable]
                   -> [Expression]
                   -> Name
                   -> [Statement]
                   -> [Statement]
buildNewstatements variables inputs1 output1 statements =
  zipWith assignInputs inputs2 inputs1 ++ statements ++ outputAssig
  where
    inputs2 = init (map stringFromVariable variables)
    outputAssig = [N output1 := Name (stringFromVariable (last variables))]

--transform program
--x = transform1 example2 []
--
inlineCalls :: LUT -> [Statement] -> [Statement]
inlineCalls lut stmts = evalState (inlineCalls' lut stmts) Map.empty

inlineCalls' :: LUT -> [Statement] -> State (Map Name Name) [Statement]
inlineCalls' lut stmts = do
  _ <- unshadow' stmts -- we only do this to set the unshadow state
  case stmts of
    [] -> pure []
    (N output := ProgramCall prog inputs:xs) -> do
      xs' <- inlineCalls' lut xs
      [Var progVars progStmts] <- unshadow' $ fromJust (lookup prog lut)
      pure $ Var progVars (buildNewstatements progVars inputs output progStmts) : xs'
    (A _i _output := ProgramCall _ _:_) -> error "we do not support array assignment for program calls"
    (Var vars body:xs) -> do
      body' <- inlineCalls' lut body
      xs' <- inlineCalls' lut xs
      pure $ Var vars body':xs'
    (If b s1 s2:xs) -> do
      s1' <- inlineCalls' lut s1
      s2' <- inlineCalls' lut s2
      xs' <- inlineCalls' lut xs
      pure $ If b s1' s2':xs'
    (While b s1:xs) -> do
      s1' <- inlineCalls' lut s1
      xs' <- inlineCalls' lut xs
      pure $ While b s1':xs'

    (x:xs) -> do
      xs' <- inlineCalls' lut xs
      pure $ x:xs'




