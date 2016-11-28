module Language.Programs where

import Language


{-
 - E(x:int | y:int) {
 - assume -1<=x ;
 - while 0<x  { x := x-1 } ; y:=x ;
 - assert y = 0
 -}
exampleE :: Program
exampleE =
  Program
    [Variable "x" (Prim Int)]
    [Variable "y" (Prim Int)]
    [ Assume (IntVal (-1))
    , While (IntVal 0 :<: Name "x") ["x" := (Name "x" :-: IntVal 1)]
    , "y" := Name "x"
    , Assert (Name "y" :=: IntVal 0)
    ]
