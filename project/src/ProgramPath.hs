module ProgramPath where

import Language
import Data.Monoid
import Debug.Trace

paths :: Int -> [Statement] -> [[Statement]]
paths n [] = [[]]
paths 0 xs = []
paths n (If e s1 s2:xs) =
  fmap (Assume e :) (paths (n - 1) (s1 <> xs)) <>
  fmap (Assume (Not e) :) (paths (n - 1) (s2 <> xs))
paths n (While e s:xs) =
  fmap ((Assume e : s) <>) (paths (n - 1) (While e s : xs)) <>
  fmap (Assume (Not e) :) (paths (n - 1) xs)
paths n (e:xs) = fmap (e :) (paths n xs)

