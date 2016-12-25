module Logic where
import Language (Expression(..), Name(..))

--" a  => (b => c)  iff  a ^ b  => c"


{-toConj :: Expression -> Expression
toConj (a :=>: (b :=>: c)) = toConj ((a :&&: b) :=>: c)
toConj (a :=>: b) = a :=>: b
toConu a = a


transform :: Expression -> Expression
transform (e1 :=>: (e2 :=>: e3)) = e1 :&&: transform (e2 :=>: e3)
transform (e1 :=>: e2) = e1 :=>: e2
transform e = e -}
