import Test.Hspec
import qualified Data.Set as Set
import qualified Data.List as List
import qualified WLP
import qualified Language.Canonical as Canonical
import Language

main :: IO ()
main = hspec $ do
  describe "WLP" $ do
      it "should rename a variable that is in a Forall" $ do
        let context = Set.fromList []
        let expr = Forall (Variable "x" (Prim Bool)) (Name "x")
        WLP.substitute "x" (Name "x") expr context `shouldSatisfy` \x ->
          (not (Set.member "x" (WLP.freeVars x)))
      it "should rename Foralls in such way that they don't conflict with bound variables" $ do
        let context = Set.fromList ["x"]
        let expr = (Name "x")
        let replBy = (Forall (Variable "x" (Prim Bool)) (Name "x"))
        (WLP.substitute "x" replBy expr context) `shouldBe` replBy



