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
    [ Assume (IntVal (-1) :<=: Name "x")
    , While (IntVal 0 :<: Name "x") ["x" := (Name "x" :-: IntVal 1)]
    , "y" := Name "x"
    , Assert (Name "y" :=: IntVal 0)
    ]

{- minind(a,i,N|r) {
 -  var min in
 -    min := a[i]
 -    r := i
 -    while i < N do {
 -      if (a[i] < min) {
 -        min := a[i]
 -        r := i
 -      } else {
 -        skip
 -      }
 -      i++
 -       
 -    }
 - }
 -}
