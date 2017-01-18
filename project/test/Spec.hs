import Test.Hspec
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
import qualified UnitTests

import System.Environment
import Control.Monad.IO.Class

main :: IO ()
main =  hspec $ do
  context "unit" $ do
    UnitTests.spec

