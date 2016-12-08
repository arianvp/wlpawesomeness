import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified WLP
import Language.Canonical

v = Variable

int = Prim Int

withVars =
  [ Var [Variable "x" int] [Var [Variable "x" int] [Assume (Name "x")]]
  , Assume (Name "x")
  ]

main :: IO ()
main =
  hspec $
  do describe "WLP.unshadow" $
       do it "should not unshadow if no bound variables" $
            do let stmts = [Var [v "x" int] []]
               WLP.unshadow stmts Map.empty `shouldBe` stmts
          it "should unshadow nested Var decls" $
            do let before = [Var [v "x" int] [Var [v "x" int] []]]
               let after = [Var [v "x" int] [Var [v "x'" int] []]]
               WLP.unshadow before Map.empty `shouldBe` after
          -- not sure if desirable
          it "should rename adjacent Vars, just to make WLP easier" $
            do let before = [Var [v "x" int] [], Var [v "x" int] []]
               let after = [Var [v "x" int] [], Var [v "x'" int] []]
               WLP.unshadow before Map.empty `shouldBe` after
          it "should keep bound vars in expressions inside statements" $
            do let before = [Var [v "x" int] [Assume (Name "x")]]
               WLP.unshadow before Map.empty `shouldBe` before
          it "should rename expressions in renamed statements" $
            do let before =
                     [Var [v "x" int] [Var [v "x" int] [Assume (Name "x")]]]
               let after =
                     [Var [v "x" int] [Var [v "x'" int] [Assume (Name "x'")]]]
               WLP.unshadow before Map.empty `shouldBe` after
          -- not sure if desirable
          it "should rename expressions in adjacent statements" $
            do let before =
                     [Var [v "x" int] [], Var [v "x" int] [Assume (Name "x")]]
               let after =
                     [Var [v "x" int] [], Var [v "x'" int] [Assume (Name "x'")]]
               WLP.unshadow before Map.empty `shouldBe` after
          it "should rename variables in Forall" $
            do let before =
                     [ Var
                         [v "x" int]
                         [Assume (Name "x" :&&: Forall (v "x" int) (Name "x"))]
                     ]
               let after =
                     [ Var
                         [v "x" int]
                         [ Assume
                             (Name "x" :&&: Forall (v "x'" int) (Name "x'"))
                         ]
                     ]
               WLP.unshadow before Map.empty `shouldBe` after
          it "should not rename bound variables of outer scope in inner scope" $
            do let before =
                     [ Var
                         [v "x" int, v "y" int]
                         [ Var
                             [v "y" int]
                             [Assume (Name "x"), Assume (Name "y")]
                         ]
                     ]
               let after =
                     [ Var
                         [v "x" int, v "y" int]
                         [ Var
                             [v "y'" int]
                             [Assume (Name "x"), Assume (Name "y'")]
                         ]
                     ]
               WLP.unshadow before Map.empty `shouldBe` after
