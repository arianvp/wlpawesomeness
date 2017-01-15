module Free where
{-import Language
import Data.Set-}

--types :: [Statement] -> Map Name Type
--types = undefined

{-free :: Expression -> [Name]
free = toList . free'
free' :: Expression -> Set Name
free' (IntVal _) = empty
free' (BoolVal _) = empty
free' (Name x) = singleton x
free' (BinOp _ y z) = free' y `union` free' z
free' (Not x) = free' x
free' (ArrayAt n i) = insert n (free' i)
free' (Forall (Variable n _) i) = delete n (free' i)-}
