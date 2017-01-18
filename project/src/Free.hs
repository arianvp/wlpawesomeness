module Free where
import Language
import Data.Set

--types :: [Statement] -> Map Name Type
--types = undefined

-- naive free variable calculator
free :: Expression -> [Expression]
free = toList . free'
free' :: Expression -> Set Expression
free' (IntVal _) = empty
free' (BoolVal _) = empty
free' (Name x) = singleton (Name x)
free' (BinOp _ y z) = free' y `union` free' z
free' (Not x) = free' x
free' (ArrayAt n i) = singleton (ArrayAt n i)
free' (Quantified (ForAll (Variable _ _)) i) = free' i
free' (Quantified (Exists (Variable _ _)) i) = free' i
free' (IfThenElseE a b c) = free' a `union` free' b `union` free' c

