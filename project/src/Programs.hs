module Programs where

import Language

exampleE :: [Statement]
exampleE =
  [ Var
      [Variable "x" (Prim Int), Variable "y" (Prim Int)]
      [ Assume (IntVal (-1) <=. Name "x")
      , While (IntVal 0 :<: Name "x") [N "x" := (Name "x" -. IntVal 1)]
      , N "y" := Name "x"
      , Assert (Name "y" =. IntVal 0)
      ]
  ]

testit :: [Statement]
testit =
  [ Var
      [Variable "x" (Prim Int), Variable "y" (Prim Int)]
      [ Var
          [Variable "y" (Prim Int)]
          [ Assume (Name "y" :<: IntVal 3)
          , N "x" := (Name "y" :-: IntVal 3)
          , Assert (Name "x" :<: IntVal 0)
          ]
      , Var
          [Variable "y" (Prim Int)]
          [ Assume (Name "y" :<: IntVal 4)
          , N "x" := (Name "y" :-: IntVal 4)
          , Assert (Name "x" :<: IntVal 0)
          ]
      ]
  ]

minind :: [Statement]
minind =
  let iinN = ((IntVal 0 :<=: Name "i") :&&: (Name "i" :<: Name "N"))
  in [ Var
         [ Variable "i" (Prim Int)
         , Variable "N" (Prim Int)
         , Variable "r" (Prim Int)
         , Variable "a" (ArrayT (Array Int))
         ]
         [ Assume iinN
         , Var
             [Variable "min" (Prim Int)]
             [ N "min" := ArrayAt (Name "a") (Name "i")
             , N "r" := Name "i"
             , While
                 (Name "i" :<: Name "N")
                 [ If
                     (ArrayAt (Name "a") (Name "i") :<: Name "min")
                     [ N "min" := ArrayAt (Name "a") (Name "i")
                     , N "r" := Name "i"
                     ]
                     [Skip]
                 , N "i" := (Name "i" :+: IntVal 1)
                 ]
             ]
         , Assert
             (forAll
                (Variable "j" (Prim Int))
                (((Name "i" :<=: Name "j") :&&: (Name "j" :<: Name "N")) :==>:
                 (ArrayAt (Name "a") (Name "r") :<=:
                  ArrayAt (Name "a") (Name "j"))))
         ]
     ]

minindWrong :: [Statement]
minindWrong =
  let iinN = ((IntVal 0 :<=: Name "i") :&&: (Name "i" :<: Name "N"))
  in [ Var
         [ Variable "i" (Prim Int)
         , Variable "N" (Prim Int)
         , Variable "r" (Prim Int)
         , Variable "a" (ArrayT (Array Int))
         ]
         [ Assume iinN
         , Var
             [Variable "min" (Prim Int)]
             [ N "min" := ArrayAt (Name "a") (Name "i")
             , N "r" := Name "i"
             , While
                 (Name "i" :<: Name "N")
                 [ If
                     (ArrayAt (Name "a") (Name "i") :<: Name "min")
                     [ N "min" := ArrayAt (Name "a") (Name "i")
                     , N "r" := Name "i"
                     ]
                     [Skip]
                 , N "i" := (Name "i" :+: IntVal 1)
                 ]
             ]
         , Assert
             (forAll
                (Variable "i" (Prim Int))
                (iinN :==>:
                 (ArrayAt (Name "a") (Name "r") :<=:
                  ArrayAt (Name "a") (Name "i"))))
         ]
     ]

-- this swap does not duplicate elements!
-- this should be discovered by our program
swapWrong :: [Statement]
swapWrong =
  [ Var
      [ Variable "a" (ArrayT (Array Int))
      , Variable "i" (Prim Int)
      , Variable "j" (Prim Int)
      , Variable "a'" (ArrayT (Array Int))
      ]
      [ Var
          [Variable "tmp" (Prim Int)]
          [ N "a'" := Name "a"
          , N "tmp" := ArrayAt (Name "a'") (Name "i")
          , A "a'" (Name "j") := Name "tmp"
          , A "a'" (Name "i") := ArrayAt (Name "a'") (Name "j")
          ]
      , Assert
          ((ArrayAt (Name "a") (Name "i") :=: ArrayAt (Name "a'") (Name "j")) :&&:
           (ArrayAt (Name "a") (Name "j") :=: ArrayAt (Name "a'") (Name "i")))
      ]
  ]

swap :: [Statement]
swap =
  [ Var
      [ Variable "a" (ArrayT (Array Int))
      , Variable "i" (Prim Int)
      , Variable "j" (Prim Int)
      , Variable "a'" (ArrayT (Array Int))
      ]
      [ Var
          [Variable "tmp" (Prim Int)]
          [ N "a'" := Name "a"
          , N "tmp" := ArrayAt (Name "a'") (Name "i")
          , A "a'" (Name "i") := ArrayAt (Name "a'") (Name "j")
          , A "a'" (Name "j") := Name "tmp"
          ]
      , Assert
          ((ArrayAt (Name "a") (Name "i") :=: ArrayAt (Name "a'") (Name "j")) :&&:
           (ArrayAt (Name "a") (Name "j") :=: ArrayAt (Name "a'") (Name "i")))
      ]
  ]
