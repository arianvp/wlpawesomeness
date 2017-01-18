module Main where
import qualified Programs
import Verify

main :: IO ()
main = do
  verify 2 2 Programs.exampleE
  putStrLn ""
  verify 2 4 Programs.minindWrong
  putStrLn ""
  verify 2 5 Programs.minind
  putStrLn ""
