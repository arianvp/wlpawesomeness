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
main = hspec $ do
  describe "Wlp.unshadow" $ do
    pure ()
  describe "Wlp.calcWlp" $ do
    pure ()
    
