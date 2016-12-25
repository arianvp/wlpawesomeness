module ProgramPath where

import Language
import Data.Monoid

paths :: Int -> [Statement] -> [[Statement]]
paths _ [] = [[]]
paths 0 _ = []
paths n (If e s1 s2:xs) =
  let
    left = fmap (Assume e :) (paths (n - 1) (s1 <> xs))
    right = fmap (Assume (Not e) :) (paths (n - 1) (s2 <> xs))
  in
    left ++ right
paths n (While e s:xs) =
  fmap
    ((Assume e : s) <>)
    (paths (n - 1) (While e s : xs)) <>
  fmap (Assume (Not e) :) (paths (n - 1) xs)

-- we assume no stuff is shadowed, so this is sound
paths n (Var _ s:xs) = paths n s <> paths n xs
paths n (e:xs) = fmap (e :) (paths n xs)

