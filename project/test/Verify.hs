{-#LANGUAGE MonadComprehensions #-}
module Verify where

import Test.Hspec

import Language
import Logic
import Wlp
import ProgramPath
import Eval
import Unshadow
import Test.Hspec.SmallCheck
import Test.SmallCheck.Series

verifyProgram :: Int -> Name -> [Statement] -> Spec
verifyProgram depth name body =
  describe ("\n" ++ name ++ "\n" ++ show body ++ "\n") $
    let
      wlps = map (normalize . flip wlp (BoolVal True)) . take 1 . paths depth . unshadow $ body
    in
      mapM_ (\x -> it ("wlp = " ++ show x) (property (evalProp x))) wlps
