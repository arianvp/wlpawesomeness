module ProgramPath where

import Language
import Data.Monoid
import Debug.Trace

paths :: Int -> [Statement] -> [[Statement]]
paths n [] = [[]]
paths 0 xs = []
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

