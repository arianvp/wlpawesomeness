module ProgramPath where

import Language
import qualified Language.Canonical as Canonical
import Data.Monoid
import Debug.Trace

paths :: Int -> [Statement] -> [[Canonical.Statement]]
paths n [] = [[]]
paths 0 xs = []
paths n (If e s1 s2:xs) =
  let
    left = fmap (Canonical.Assume e :) (paths (n - 1) (s1 <> xs))
    right = fmap (Canonical.Assume (Canonical.Not e) :) (paths (n - 1) (s2 <> xs))
  in
    left ++ right
paths n (While e s:xs) =
  fmap
    ((Canonical.Assume e : map toCanonical s) <>)
    (paths (n - 1) (While e s : xs)) <>
  fmap (Canonical.Assume (Canonical.Not e) :) (paths (n - 1) xs)

-- we assume no stuff is shadowed, so this is sound
paths n (Var _ s:xs) = paths n s <> paths n xs
paths n (e:xs) = fmap (toCanonical e :) (paths n xs)

toCanonical :: Statement -> Canonical.Statement
toCanonical Skip = Canonical.Skip
toCanonical (Assume e) = Canonical.Assume e
toCanonical (Assert e) = Canonical.Assert e
toCanonical (a := e) = (Canonical.:=) a e
toCanonical (Var v s) = Canonical.Var v (map toCanonical s)
toCanonical x = error $ "tried to cannon " ++ show x
