import Data.Maybe
import Language

example1 :: [Statement]
example1 =
  [ Var
      [Variable "k" (Prim Int),Variable "m" (Prim Int), Variable "r" (Prim Int)]
      [N "k" := (Name "k" :+: IntVal 1 )
	  ,N "r" := (Name "k" :+: Name "m" )
      ]
  ]
  
example2 :: [Statement]
example2 =
  [ Var
      [Variable "c" (Prim Int),Variable "p" (Prim Int), Variable "d" (Prim Int)]
      [N "c" := IntVal 3
	  ,N "p" := IntVal 1
      ,N "d" := ProgramCall "example1" [(Name "c"  :+: IntVal 1),Name "p" ]
	  ,N "p" := Name "d"
      ]
  ]



lookupTable = [("example1",  example1)]

assignInputs:: String->Expression->Statement
assignInputs s1 s2 = N s1 := s2

stringFromVariable :: Variable -> String
stringFromVariable (Variable n t) = n

--finds ProgramCall and replace it using replacePC function, already used variables stored in usedVar

transform1 :: [Statement] -> [Variable]->[Statement]
transform1 ((Var a b):stmts) usedVar = [Var a (transform1 b (usedVar ++ a))] ++ transform1 stmts (usedVar ++ a) --
transform1 (((:=) tag (ProgramCall pr exp)):stmts) usedVar = (replacePC tag pr exp usedVar) ++ transform1 stmts usedVar
transform1 (stmt:stmts) usedVar = [stmt] ++ transform1 stmts usedVar
transform1 [] usedVar = []


replacePC:: AsgTarget -> Name -> [Expression] -> [Variable]-> [Statement]
replacePC (N output1) progName inputs1 usedVar = [Var variables (buildNewstatements variables inputs1 output1 statements) ]
	where ((Var variables statements):stmts) = fromJust (lookup progName lookupTable) -- TODO:: this are statements of program we are calling, we should first unshadow this using usedVar

--newstatements is [assign input, body, assign output]

buildNewstatements:: [Variable]->[Expression]->Name ->[Statement]->[Statement]
buildNewstatements variables inputs1 output1 statements= (zipWith assignInputs inputs2 inputs1) ++ statements ++ outputAssig
	where inputs2=init (map stringFromVariable variables)
	      outputAssig = [N output1 := Name (stringFromVariable (last variables))]
		  
--transform program
x = transform1 example2 []
