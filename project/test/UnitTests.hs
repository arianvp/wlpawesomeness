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
import qualified Logic
import qualified Series
import qualified Eval

noQuantifier (Quantified _ _) = False
noQuantifier (Quantified _ _) = False
noQuantifier _ = True

spec :: Spec
spec = do
  describe "Logic.prenex" $ do
    let x = Variable "x" (Prim Int)
    let z = Variable "z" (Prim Int)
    let a = Name "a"
    let b = Name "b"
    let c = Name "c"
    it "the wikipedia example works" $ do
      let
        before =
          (a :||: (exists x b)) :==>: (forAll z c)
      (all noQuantifier . universe . Logic.strip . Logic.prenex $ before) `shouldBe` True

    it "Another example works" $ do
      let
        before =
          BoolVal True :==>: (forAll (Variable "i" (Prim Int)) (Name "i" :=: Name "i") :&&: BoolVal True)
      Logic.prenex before `shouldSatisfy` all noQuantifier . universe . Logic.strip

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
                      [ "x" := IntVal 1
                      , "y" := IntVal 3
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
                      [ "x" := IntVal 1
                      , "y" := IntVal 3
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
  describe "Wlp.wlp" $
    do it "sequential composition is correct for arrays" $
         do let before = ["r" := Name "i", Assert (ArrayAt "a" (Name "r"))]
                after = ArrayAt "a" (Name "i") :&&: BoolVal True
            Wlp.wlp before (BoolVal True) `shouldBe` after
  {-let firstUnrolling = head . ProgramPath.paths 3 . Unshadow.unshadow $ Programs.minind
      let wlp = Logic.normalize . flip Wlp.wlp (BoolVal True) $ firstUnrolling
      it (show wlp) $ do
        let env = [ (ArrayAt "a" (Name "i"), Series.ints)
                  , (Name "i", Series.ints)
                  , (Name "N", Series.ints) ]
        property $ Property.prop env wlp
      -}
  describe "Eval" $ do
    it "only tries relevant test cases" $ do
      let
        prop =
          forAll (Variable "i" (Prim Int)) $
            (IntVal 0 :<=: Name "i") :==>: ((IntVal 0 :<: Name "i") :||: (Name "i" :=: IntVal 0))
      property $ Eval.evalProp prop
    it "evalArrays should get rid of non-evaluated expressions" $ do
      let
        before =
          (ArrayAt "x" (IntVal 3 :+: IntVal 4)) :+:
          (ArrayAt "x" (IntVal 4 :+: IntVal 5))
        after =
          (ArrayAt "x" (IntVal 7)) :+: (ArrayAt "x" (IntVal 9))
      Eval.evalArrays "x" before `shouldBe` after
    it "should be able to quickcheck a simple array" $ do
      let
        wlp =
          forAll (Variable "a" (ArrayT (Array Int))) $
          forAll (Variable "i" (Prim Int)) $
            ArrayAt "a" (Name "i") :=: ArrayAt "a" (Name "i")
      property $ Eval.evalProp wlp
    it "should be able to quickcheck a simple array'" $ do
      let
        wlp =
          forAll (Variable "i" (Prim Int)) $
          forAll (Variable "a" (ArrayT (Array Int))) $
            ArrayAt "a" (Name "i") :=: ArrayAt "a" (Name "i")
      property $ Eval.evalProp wlp
    it "the form of the expr doesnt matter as long as it evaluates [1]" $ do
      let
        wlp =
          forAll (Variable "i" (Prim Int)) $
          forAll (Variable "a" (ArrayT (Array Int))) $
            (ArrayAt "a" (IntVal 1 :+: Name "i") :=: ArrayAt "a" (Name "i" :+: IntVal 1))
      property $ Eval.evalProp wlp
    it "the order of forall does matter [1] " $ do
      let
        wlp =
          forAll (Variable "a" (ArrayT (Array Int))) $
          forAll (Variable "i" (Prim Int)) $
            (ArrayAt "a" (IntVal 1 :+: Name "i") :=: ArrayAt "a" (Name "i" :+: IntVal 1))
      property $ Eval.evalProp wlp
    it "the form of the expr doesnt matter as long as it evaluates [2]" $ do
      let
        wlp =
          forAll (Variable "i" (Prim Int)) $
          forAll (Variable "a" (ArrayT (Array Int))) $
            (ArrayAt "a" (Name "i" :+: IntVal 1) :=: ArrayAt "a" (IntVal 1 :+: Name "i"))
      property $ Eval.evalProp wlp
    it "the order of forall does matter! [2] " $ do
      let
        wlp =
          forAll (Variable "a" (ArrayT (Array Int))) $
          forAll (Variable "i" (Prim Int)) $
            (ArrayAt "a" (Name "i" :+: IntVal 1) :=: ArrayAt "a" (IntVal 1 :+: Name "i"))
      property $ Eval.evalProp wlp

    it "should be able to quickcheck multiple simple array'" $ do
      let
        wlp =
          forAll (Variable "i" (Prim Int)) $
          forAll (Variable "a" (ArrayT (Array Int))) $
          forAll (Variable "b" (ArrayT (Array Int))) $
            (ArrayAt "a" (Name "i") :=: ArrayAt "a" (Name "i")) :&&:
            (ArrayAt "b" (Name "j") :=: ArrayAt "b" (Name "j"))
      property $ Eval.evalProp wlp
    it "foralls should not cause any issues" $ do
      let
        wlp =
          forAll (Variable "a" (ArrayT (Array Int))) $
            (ArrayAt "a" (IntVal 0) :=: IntVal 0) :&&:
              (forAll (Variable "i" (Prim Int))
                ((Name "i" :=: IntVal 0) :==>: (ArrayAt "a" (Name "i") :=: IntVal 0)))
      property $ Eval.evalProp wlp

