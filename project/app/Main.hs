module Main where
import qualified Programs
import Verify

main :: IO ()
main = do
  verify 3 5 Programs.exampleE
  putStrLn "minindWrong (SHOULD FAIL):"
  verify 3 5 Programs.minindWrong
  putStrLn "minind (SHOULD SUCCEED):"
  verify 3 5 Programs.minind
  putStrLn "swapWrong (SHOULD FAIL):"
  verify 3 5 Programs.swapWrong
  putStrLn "swap (SHOULD SUCCEED):"
  verify 3 5 Programs.swap

