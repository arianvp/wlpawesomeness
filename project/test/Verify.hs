module Verify where

import Test.Hspec

import Language
import Logic
import Wlp
import Property
import ProgramPath
import Unshadow

verifyProgram :: Name -> [Statement] -> Spec
verifyProgram name body =
  describe ("\n" ++ name ++ "\n" ++ show body ++ "\n") $
    let
      wlps = map (normalize . flip wlp (BoolVal True)) . paths 20 . unshadow $ body
    in
      mapM_ (\x -> it ("wlp = " ++ show x) (prop [("x", intGen)] x)) wlps
