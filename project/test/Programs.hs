module Programs where

import Test.Hspec
import Language
import Verify

exampleE :: [Statement]
exampleE =
  [ Var
      [Variable "x" (Prim Int), Variable "y" (Prim Int)]
      [ Assume (IntVal (-1) <=. Name "x")
      , While  (IntVal 0 :<: Name "x")
        ["x" := (Name "x" -. IntVal 1)]
      , "y" := Name "x"
      , Assert (Name "y" =. IntVal 0)
      ]
  ]

testit =
  [ Var
      [Variable "x" (Prim Int), Variable "y" (Prim Int)]
      [ Var
          [Variable "y" (Prim Int)]
          [ Assume (Name "y" :<: IntVal 3)
          , "x" := (Name "y" :-: IntVal 3)
          , Assert (Name "x" :<: IntVal 0)
          ]
      , Var
          [Variable "y" (Prim Int)]
          [ Assume (Name "y" :<: IntVal 4)
          , "x" := (Name "y" :-: IntVal 4)
          , Assert (Name "x" :<: IntVal 0)
          ]
      ]
  ]

minind =
  [ Var
      [ Variable "a" (ArrayT (Array Int))
      , Variable "i" (Prim Int)
      , Variable "N" (Prim Int)
      , Variable "r" (Prim Int)
      ]
      [ Var
          [Variable "min" (Prim Int)]
          [ "min" := ArrayAt "a" (Name "i")
          , "r" := Name "i"
          , While
              (Name "i" :<: Name "N")
              [ If
                  (ArrayAt "a" (Name "i") :<: Name "min")
                  ["min" := ArrayAt "a" (Name "i"), "r" := Name "i"]
                  [Skip]
              ]
          ]
      ]
  ]

spec :: Spec
spec = do
  verifyProgram "exampleE" exampleE
