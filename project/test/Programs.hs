module Programs where

import Test.Hspec
import Language
import Verify
import qualified Series

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
  let iinN = ((IntVal 0 :<=: Name "i") :&&: (Name "i" :<: Name "N"))
  in
  [ Var
      [ Variable "i" (Prim Int)
      , Variable "N" (Prim Int)
      , Variable "r" (Prim Int)
      , Variable "a" (ArrayT (Array Int))
      ]
      [ Assume iinN
      , Var
          [Variable "min" (Prim Int)]
          [ "min" := ArrayAt "a" (Name "i")
          , "r" := Name "i"
          , While
              (Name "i" :<: Name "N")
              [ If
                  (ArrayAt "a" (Name "i") :<: Name "min")
                  ["min" := ArrayAt "a" (Name "i"), "r" := Name "i"]
                  [Skip]
              , "i" := (Name "i" :+: IntVal 1)
              ]
          ]
      , Assert (Forall (Variable "i" (Prim Int)) (iinN :==>: (ArrayAt "a" (Name "r") :<=: ArrayAt "a" (Name "i"))))
      ]
  ]

-- the programs to verify
spec :: Spec
spec = do
  verifyProgram 5 "a test if forall works" 
    [ Var [Variable "x" (Prim Int)]
      [ Assume (Name "x" :=: IntVal 0)
      , Assert (Forall (Variable "y" (Prim Int)) ((IntVal 0 :<: Name "y" ) :==>: ((Name "y" :=: Name "x"))))
      ]
    ]
  -- TODO write a function freeVars, that automatically gives the series
  -- of a wlp
  verifyProgram 5 "exampleE" exampleE
  {-verifyProgram
    5
    "minind"
    [ (Name "x", Series.ints)
    , (ArrayAt "a" (Name "i") , Series.ints)
    , (ArrayAt "a" (Name "r") , Series.ints)
    , (Name "i", Series.ints)
    , (Name "N", Series.ints)
    , (Name "r", Series.ints)
    ]
    minind
  -}
  verifyProgram 15 "minind" minind
  
