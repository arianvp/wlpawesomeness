{-#LANGUAGE MonadComprehensions #-}
module Verify where

import Test.Hspec

import Language
import Logic
import Wlp
import Property
import ProgramPath
import Unshadow
import Test.Hspec.SmallCheck
import Test.SmallCheck.Series

verifyProgram :: Int -> Name -> [(Name, Series IO Expression)] -> [Statement] -> Spec
verifyProgram depth name series' body =
  describe ("\n" ++ name ++ "\n" ++ show body ++ "\n") $
    let
      wlps = map (normalize . flip wlp (BoolVal True)) . paths depth . unshadow $ body
    in
      mapM_ (\x -> it ("wlp = " ++ show x) (property (prop series' x))) wlps
