import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Wlp
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
  describe "Wlp.unshadow" $ do
    pure ()
  describe "Wlp.calcWlp" $ do
    pure ()
