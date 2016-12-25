{-#LANGUAGE PatternSynonyms #-}
module Logic where
import Language


normalize :: Expression -> Expression
normalize (e1 :==>: (e2 :==>: e3)) = e1 :&&: normalize (e2 :==>: e3)
normalize (e1 :==>: e2) = e1 :==>: e2
normalize e = e
