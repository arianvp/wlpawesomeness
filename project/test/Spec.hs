import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Wlp
import qualified Unshadow
import Language
import Data.Generics.Uniplate.Operations

exampleE :: Program
exampleE =
  Program
    [ Variable "x" (Prim Int) ]
    [ Variable "y" (Prim Int) ]
    [ Assume (IntVal (-1) <=. Name "x")
    , While (IntVal 0 <. Name "x")
      [ "x" := (Name "x" -. IntVal 1) ]
    , "y" := Name "x"
    , Assert (Name "y" =. IntVal 0)
    ]

main :: IO ()
main = hspec $ do
  describe "Unshadow.unshadow" $ do
    it "exampleE has no shadowed variables" $ do
      Unshadow.unshadowProgram exampleE `shouldBe` exampleE
    it "an example of unshadowing" $ do
      let
        before =
          Program [Variable "x" (Prim Int)] [Variable "y" (Prim Int)]
          ["x" := IntVal 1
          ,"y" := IntVal 3
          , Var [Variable "y" (Prim Int)]
            [ Assert (Name "x")
            , Var [Variable "y" (Prim Int)]
              [ Assert (Name "y") ]
            ]
          ]
        after =
          Program [Variable "x" (Prim Int)] [Variable "y" (Prim Int)]
          ["x" := IntVal 1
          ,"y" := IntVal 3
          , Var [Variable "y'" (Prim Int)]
            [ Assert (Name "x")
            , Var [Variable "y''" (Prim Int)]
              [ Assert (Name "y''") ]
            ]
          ]
      Unshadow.unshadowProgram before `shouldBe` after
    it "Vars are independent, they dont cause troubles in wlp, I think" $ do
      let
        before =
          Program [Variable "x" (Prim Int)] [Variable "y" (Prim Int)]
          [ Var [Variable "y" (Prim Int)] []
          , Var [Variable "y" (Prim Int)] []
          ]
        after =
          Program [Variable "x" (Prim Int)] [Variable "y" (Prim Int)]
          [ Var [Variable "y'" (Prim Int)] []
          , Var [Variable "y'" (Prim Int)] []
          ]
      Unshadow.unshadowProgram before `shouldBe` after

  describe "ProgramPath.paths" $ do
    pure ()
  describe "Wlp.calcWlp" $ do
    pure ()
