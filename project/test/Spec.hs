import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Wlp
import qualified Unshadow
import qualified ProgramPath
import Language
import Data.Generics.Uniplate.Operations

exampleE :: [Statement]
exampleE =
    [ Var [Variable "x" (Prim Int) ,Variable "y" (Prim Int)]
      [ Assume (IntVal (-1) <=. Name "x")
      , "x" := (Name "x" -. IntVal 1)
      , "y" := Name "x"
      , Assert (Name "y" =. IntVal 0)
      ]
    ]

testit =
  [ Var [Variable "x" (Prim Int) ,Variable "y" (Prim Int)]
    [ Var [Variable "y" (Prim Int)]
      [ Assume (Name "y" :<: IntVal 3)
      , "x" := (Name "y" :-: IntVal 3)
      , Assert (Name "x" :<: IntVal 0)
      ]
    , Var [Variable "y" (Prim Int)]
      [ Assume (Name "y" :<: IntVal 4)
      , "x" := (Name "y" :-: IntVal 4)
      , Assert (Name "x" :<: IntVal 0)
      ]
    ]
  ]

main :: IO ()
main = hspec $ do
  describe "Unshadow.unshadow" $ do
    it "exampleE has no shadowed variables" $ do
      Unshadow.unshadow exampleE `shouldBe` exampleE
    it "an example of unshadowing" $ do
      let
        pbefore =
          [ Var [Variable "x" (Prim Int) ,Variable "y" (Prim Int)]
            ["x" := IntVal 1
            ,"y" := IntVal 3
            , Var [Variable "y" (Prim Int)]
              [ Assert (Name "x")
              , Var [Variable "y" (Prim Int)]
                [ Assert (Name "y") ]
              ]
            ]
          ]
        pafter =
          [ Var [Variable "x" (Prim Int) ,Variable "y" (Prim Int)]
            ["x" := IntVal 1
            ,"y" := IntVal 3
            , Var [Variable "y'" (Prim Int)]
              [ Assert (Name "x")
              , Var [Variable "y''" (Prim Int)]
                [ Assert (Name "y''") ]
              ]
            ]
          ]
      Unshadow.unshadow pbefore `shouldBe` pafter
    it "Adjacent vars should be renamed" $ do
      let
        pbefore =
          [ Var [Variable "x" (Prim Int) ,Variable "y" (Prim Int)]
            [ Var [Variable "y" (Prim Int)] []
            , Var [Variable "z" (Prim Int)] []
            , Var [Variable "y" (Prim Int)] []
            ]
          ]
        pafter =
          [ Var [Variable "x" (Prim Int) ,Variable "y" (Prim Int)]
            [ Var [Variable "y'" (Prim Int)] []
            , Var [Variable "z" (Prim Int)] []
            , Var [Variable "y''" (Prim Int)] []
            ]
          ]
      Unshadow.unshadow pbefore `shouldBe` pafter

  describe "ProgramPath.paths" $ do
    it "should remove vars" $ do
      let
        pbefore =
          [ Var [Variable "x" (Prim Int) ,Variable "y" (Prim Int)]
            [ Var [Variable "y'" (Prim Int)]
              [ Assume (Name "y'" :<: IntVal 3)
              , "x" := (Name "y'" :-: IntVal 3)
              , Assert (Name "x" :<: IntVal 0)
              ]
            , Var [Variable "y''" (Prim Int)]
              [ Assume (Name "y''" :<: IntVal 4)
              , "x" := (Name "y''" :-: IntVal 4)
              , Assert (Name "x" :<: IntVal 0)
              ]
            ]
          ]
        pafter =
            [ Assume (Name "y'" :<: IntVal 3)
            , "x" := (Name "y'" :-: IntVal 3)
            , Assert (Name "x" :<: IntVal 0)
            , Assume (Name "y''" :<: IntVal 4)
            , "x" := (Name "y''" :-: IntVal 4)
            , Assert (Name "x" :<: IntVal 0)
            ]
      ProgramPath.paths 1 pbefore `shouldContain` [pafter]
  describe "Wlp.calcWlp" $ do
    pure ()
