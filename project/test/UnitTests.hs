-- | A collection of tests to check if
-- functionality we implemented is correct
module UnitTests where

import Test.Hspec
import Test.Hspec.SmallCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Wlp
import qualified Unshadow
import qualified ProgramPath
import Language
import Data.Generics.Uniplate.Operations

import Verify
import qualified Programs
import qualified ProgramCall
import qualified Logic
import qualified Eval

noQuantifier (Quantified _ _) = False
noQuantifier _ = True

spec :: Spec
spec = do
  describe "Logic.sortedPrenex" $ do
    let x = Variable "x" (Prim Int)
    let z = Variable "z" (Prim Int)
    let a = Name "a"
    let b = Name "b"
    let c = Name "c"
    it "the wikipedia example works" $ do
      let
        before =
          (a :||: (exists x b)) :==>: (forAll z c)
      (all noQuantifier . universe . Logic.strip . Logic.sortedPrenex $ before) `shouldBe` True

    it "Another example works" $ do
      let
        before =
          BoolVal True :==>: (forAll (Variable "i" (Prim Int)) (Name "i" :=: Name "i") :&&: BoolVal True)
      Logic.sortedPrenex before `shouldSatisfy` all noQuantifier . universe . Logic.strip

  describe "Unshadow.unshadow" $
    do it "exampleE has no shadowed variables" $
         Unshadow.unshadow Programs.exampleE `shouldBe` Programs.exampleE
       it "should unshadow quantifiers" $
         do let pbefore =
                  [ Var
                      [Variable "i" (Prim Int)]
                      [Assert (forAll (Variable "i" (Prim Bool)) (Name "i"))]
                  ]
                pafter =
                  [ Var
                      [Variable "i" (Prim Int)]
                      [Assert (forAll (Variable "i'" (Prim Bool)) (Name "i'"))]
                  ]
            Unshadow.unshadow pbefore `shouldBe` pafter
       it "an example of unshadowing" $
         do let pbefore =
                  [ Var
                      [Variable "x" (Prim Int), Variable "y" (Prim Int)]
                      [ N "x" := IntVal 1
                      , N "y" := IntVal 3
                      , Var
                          [Variable "y" (Prim Int)]
                          [ Assert (Name "x")
                          , Var [Variable "y" (Prim Int)] [Assert (Name "y")]
                          ]
                      ]
                  ]
                pafter =
                  [ Var
                      [Variable "x" (Prim Int), Variable "y" (Prim Int)]
                      [ N "x" := IntVal 1
                      , N "y" := IntVal 3
                      , Var
                          [Variable "y'" (Prim Int)]
                          [ Assert (Name "x")
                          , Var
                              [Variable "y''" (Prim Int)]
                              [Assert (Name "y''")]
                          ]
                      ]
                  ]
            Unshadow.unshadow pbefore `shouldBe` pafter
       it "Adjacent vars should be renamed" $
         do let pbefore =
                  [ Var
                      [Variable "x" (Prim Int), Variable "y" (Prim Int)]
                      [ Var [Variable "y" (Prim Int)] []
                      , Var [Variable "z" (Prim Int)] []
                      , Var [Variable "y" (Prim Int)] []
                      ]
                  ]
                pafter =
                  [ Var
                      [Variable "x" (Prim Int), Variable "y" (Prim Int)]
                      [ Var [Variable "y'" (Prim Int)] []
                      , Var [Variable "z" (Prim Int)] []
                      , Var [Variable "y''" (Prim Int)] []
                      ]
                  ]
            Unshadow.unshadow pbefore `shouldBe` pafter
  describe "ProgramPath.paths" $
    do it "should remove an if in a while, and remove the while" $
         do let pbefore =
                  [ While
                      (Name "i" :<: Name "N")
                      [ If
                          (Name "x" :<: Name "k")
                          [Assert (Name "left")]
                          [Assert (Name "right")]
                      ]
                  ]
            (concat . concat . universe . ProgramPath.paths 4 $ pbefore) `shouldSatisfy`
              all
                (\x ->
                    case x of
                      If {} -> False
                      While {} -> False
                      _ -> True)
       it "should remove a while in an if, and remove the if" $
         do let pbefore =
                  [ If
                      (Name "i" :<: Name "N")
                      [While (Name "x" :<: Name "k") [Assert (Name "left")]]
                      [While (Name "x" :<: Name "k") [Assert (Name "left")]]
                  ]
            (concat . concat . universe . ProgramPath.paths 4 $ pbefore) `shouldSatisfy`
              all
                (\x ->
                    case x of
                      If {} -> False
                      While {} -> False
                      _ -> True)
  describe "Wlp.wlp" $ do
    it "sequential composition is correct for arrays" $ do
      let before = [N "r" := Name "i", Assert (ArrayAt (Name "a") (Name "r"))]
      let after = ArrayAt (Name "a") (Name "i") :&&: BoolVal True
      Wlp.wlp before (BoolVal True) `shouldBe` after
    it "the array assignment example from lecture notes works" $ do
      let prog = [A "a" (Name "i") := IntVal 0]
      let postc =  ArrayAt (Name "a") (Name "i") :=: ArrayAt (Name "a") (IntVal 3)
      let wlp = IntVal 0 :=: (IfThenElseE  (IntVal 3 :=: Name "i") (IntVal 0) (ArrayAt (Name "a") (IntVal 3)))
      Eval.reduce (Wlp.wlp prog postc) `shouldBe` wlp

  describe "Eval" $ do
    it "only tries relevant test cases" $ do
      let
        prop =
          forAll (Variable "i" (Prim Int)) $
            (IntVal 0 :<=: Name "i") :==>: ((IntVal 0 :<: Name "i") :||: (Name "i" :=: IntVal 0))
      property .  Eval.evalProp' . Logic.sortedPrenex $ prop
    it "should be able to quickcheck a simple array" $ do
      let
        wlp =
          forAll (Variable "a" (ArrayT (Array Int))) $
          forAll (Variable "i" (Prim Int)) $
            ArrayAt (Name "a") (Name "i") :=: ArrayAt (Name "a") (Name "i")
      property .  Eval.evalProp' . Logic.sortedPrenex $ wlp
    it "should be able to quickcheck a simple array'" $ do
      let
        wlp =
          forAll (Variable "i" (Prim Int)) $
          forAll (Variable "a" (ArrayT (Array Int))) $
            ArrayAt (Name "a") (Name "i") :=: ArrayAt (Name "a") (Name "i")
      property . Eval.evalProp' . Logic.sortedPrenex $ wlp
    it "the form of the expr doesnt matter as long as it evaluates [1]" $ do
      let
        wlp =
          forAll (Variable "i" (Prim Int)) $
          forAll (Variable "a" (ArrayT (Array Int))) $
            (ArrayAt (Name "a") (IntVal 1 :+: Name "i") :=: ArrayAt (Name "a") (Name "i" :+: IntVal 1))
      property . Eval.evalProp' . Logic.sortedPrenex $ wlp
    it "the order of forall does not matter [1] " $ do
      let
        wlp =
          forAll (Variable "a" (ArrayT (Array Int))) $
          forAll (Variable "i" (Prim Int)) $
            (ArrayAt (Name "a") (IntVal 1 :+: Name "i") :=: ArrayAt (Name "a") (Name "i" :+: IntVal 1))
      property . Eval.evalProp' . Logic.sortedPrenex $ wlp
    it "the form of the expr doesnt matter as long as it evaluates [2]" $ do
      let
        wlp =
          forAll (Variable "i" (Prim Int)) $
          forAll (Variable "a" (ArrayT (Array Int))) $
            (ArrayAt (Name "a") (Name "i" :+: IntVal 1) :=: ArrayAt (Name "a") (IntVal 1 :+: Name "i"))
      property . Eval.evalProp' . Logic.sortedPrenex $ wlp
    it "the order of forall does not matter! [2] " $ do
      let
        wlp =
          forAll (Variable "a" (ArrayT (Array Int))) $
          forAll (Variable "i" (Prim Int)) $
            (ArrayAt (Name "a") (Name "i" :+: IntVal 1) :=: ArrayAt (Name "a") (IntVal 1 :+: Name "i"))
      property . Eval.evalProp' . Logic.sortedPrenex $ wlp

    it "should be able to quickcheck multiple simple array'" $ do
      let
        wlp =
          forAll (Variable "i" (Prim Int)) $
          forAll (Variable "a" (ArrayT (Array Int))) $
          forAll (Variable "b" (ArrayT (Array Int))) $
            (ArrayAt (Name "a") (Name "i") :=: ArrayAt (Name "a") (Name "i")) :&&:
            (ArrayAt (Name "b") (Name "j") :=: ArrayAt (Name "b") (Name "j"))
      property . Eval.evalProp' . Logic.sortedPrenex $ wlp
    it "foralls should not cause any issues" $ do
      let
        wlp =
          forAll (Variable "a" (ArrayT (Array Int))) $
            (ArrayAt (Name "a") (IntVal 0) :=: IntVal 0) :==>:
              (forAll (Variable "i" (Prim Int))
                ((Name "i" :=: IntVal 0) :==>: (ArrayAt (Name "a") (Name "i") :=: IntVal 0)))
      property . Eval.evalProp' . Logic.sortedPrenex $ wlp
    it "ifthenelse works as expected [1]" $ do
      let x = IfThenElseE (BoolVal True) (Name "x") (Name "y")
      Eval.reduce x `shouldBe` Name "x"
    it "ifthenelse works as expected [2]" $ do
      let x = IfThenElseE (BoolVal False) (Name "x") (Name "y")
      Eval.reduce x `shouldBe` Name "y"
  describe "ProgramCall" $ do
    it "programs without program calls are unaffected" $ do
      ProgramCall.inlineCalls [] Programs.minind `shouldBe` Programs.minind
    it "should inline one call into the other." $ do
      let
        prog =
          [ Var
              [Variable "k" (Prim Int), Variable "r" (Prim Int)]
              [N "r" := (Name "k" :+: IntVal 1)]
          ]
        lut = [("prog", prog)]
        caller =
          [ Var
              [Variable "a" (Prim Int), Variable "b" (Prim Int)]
              [ N "b" := ProgramCall "prog" [Name "a"]]
          ]
        result =
          [ Var
              [Variable "a" (Prim Int), Variable "b" (Prim Int)]
              [ Var
                  [Variable "k" (Prim Int), Variable "r" (Prim Int)]
                  [ N "k" := Name "a"
                  , N "r" := (Name "k" :+: IntVal 1)
                  , N "b" := Name "r"
                  ]
              ]
          ]
      ProgramCall.inlineCalls lut caller `shouldBe` result
    it "should make sure that the inlined function is fresh." $ do
      let
        prog =
          [ Var
              [Variable "k" (Prim Int), Variable "r" (Prim Int)]
              [N "r" := (Name "k" :+: IntVal 1)]
          ]
        lut = [("prog", prog)]
        caller =
          [ Var
              [Variable "k" (Prim Int), Variable "r" (Prim Int)]
              [ N "r" := ProgramCall "prog" [Name "k"]]
          ]
        result =
          [ Var
              [Variable "k" (Prim Int), Variable "r" (Prim Int)]
              [ Var
                  [Variable "k'" (Prim Int), Variable "r'" (Prim Int)]
                  [ N "k'" := Name "k"
                  , N "r'" := (Name "k'" :+: IntVal 1)
                  , N "r" := Name "r'"
                  ]
              ]
          ]
      ProgramCall.inlineCalls lut caller `shouldBe` result
    it "should work on nested statements" $ do
      let
        prog =
          [ Var
              [Variable "k" (Prim Int), Variable "r" (Prim Int)]
              [N "r" := (Name "k" :+: IntVal 1)]
          ]
        lut = [("prog", prog)]
        caller =
          [ Var
              [Variable "a" (Prim Int), Variable "b" (Prim Int)]
              [ If (BoolVal True)
                  [N "b" := ProgramCall "prog" [Name "a"]]
                  [Skip]
              ]
          ]
        result =
          [ Var
              [Variable "a" (Prim Int), Variable "b" (Prim Int)]
              [ If (BoolVal True)
                  [ Var
                      [Variable "k" (Prim Int), Variable "r" (Prim Int)]
                      [ N "k" := Name "a"
                      , N "r" := (Name "k" :+: IntVal 1)
                      , N "b" := Name "r"
                      ]
                  ]
                  [Skip]
              ]
          ]
      ProgramCall.inlineCalls lut caller `shouldBe` result
    it "works for a program with more than two variables" $ do
      let
        prog =
          [ Var
              [ Variable "a" (Prim Int)
              , Variable "b" (Prim Int)
              , Variable "c" (Prim Int)
              , Variable "d" (Prim Int)
              ]
              [ N "b" := Name "a"
              , N "c" := Name "b"
              , N "d" := Name "c"
              ]
          ]
        lut = [("prog", prog)]
        caller =
          [ Var
              [ Variable "a" (Prim Int)
              , Variable "b" (Prim Int)
              , Variable "c" (Prim Int)
              , Variable "d" (Prim Int)
              ]
              [ N "d" := ProgramCall "prog" [Name "a", Name "b", Name "c"] ]
          ]
        result =
          [ Var
              [ Variable "a" (Prim Int)
              , Variable "b" (Prim Int)
              , Variable "c" (Prim Int)
              , Variable "d" (Prim Int)
              ]
            [ Var
                [ Variable "a'" (Prim Int)
                , Variable "b'" (Prim Int)
                , Variable "c'" (Prim Int)
                , Variable "d'" (Prim Int)
                ]
                [ N "a'" := Name "a"
                , N "b'" := Name "b"
                , N "c'" := Name "c"
                , N "b'" := Name "a'"
                , N "c'" := Name "b'"
                , N "d'" := Name "c'"
                , N "d" := Name "d'"
                ]
            ]
          ]

      ProgramCall.inlineCalls lut caller `shouldBe` result
    it "A program that is simply a shim for another should have an identical wlp" $ do
      let
        lut = [("swap", Programs.swap)]
        prog =
          [ Var
              [ Variable "a" (ArrayT (Array Int))
              , Variable "i" (Prim Int)
              , Variable "j" (Prim Int)
              , Variable "a'" (ArrayT (Array Int))
              ]
              [ N "a'" := ProgramCall "swap" [Name "a", Name "i", Name "j"]]
          ]

      Logic.normalize (Wlp.wlp (ProgramCall.inlineCalls lut prog) (BoolVal True))
      `shouldBe` Logic.normalize (Wlp.wlp Programs.swap (BoolVal True))



