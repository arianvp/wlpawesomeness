module ProgramPath where

import Language
import Data.Monoid
import Data.List (nub)

paths :: Int -> [Statement] -> [[Statement]]
paths n = reverse . paths' n

paths' :: Int -> [Statement] -> [[Statement]]
paths' _ [] = [[]]
paths' 0 _ = []
paths' n (If e s1 s2:xs) =
  let left = fmap (Assume e :) (paths' (n - 1) (s1 <> xs))
      right = fmap (Assume (Not e) :) (paths' (n - 1) (s2 <> xs))
  in left ++ right
paths' n (While e s:xs) =
  nub $ do
    y <- paths' (n-1) s
    fmap ((Assume e : y) <>) (paths' (n - 1) (While e y : xs)) <>
      fmap (Assume (Not e) :) (paths' (n - 1) xs)
paths' n (Var _ s:xs) = do
  x <- paths' n s
  y <- paths' n xs
  pure (x <> y)
paths' n (e:xs) = fmap (e :) (paths' n xs)
