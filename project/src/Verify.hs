{-#LANGUAGE MonadComprehensions #-}
module Verify where


import Language
import Logic
import Wlp
import ProgramPath
import Eval
import Unshadow
import Test.SmallCheck.Drivers



verify :: Int -> Int -> [Statement] -> IO ()
verify smallCheckDepth pathsDepth program = do
  putStrLn "I am now testing your program:"
  print program
  putStrLn "with the following parameters:"
  putStrLn $ "\t smallCheckDepth = " ++ show smallCheckDepth
  putStrLn $ "\t pathsDepth = " ++ show pathsDepth

  let unshadowed = unshadow program
  let ps = paths pathsDepth unshadowed
  let wlps = map (normalize . flip wlp (BoolVal True)) ps

  putStrLn "Hold on your butts! this might take a while"
  maybeFailure <- test' wlps
  case maybeFailure of
    Just failure -> putStrLn $ ppFailure failure
    Nothing -> putStrLn "Completed!"

  where
    test' [] = error "there was nothing to test at this depth"
    test' (x:[]) = do
      putStrLn $ "smallchecking: " ++ show x
      smallCheckM smallCheckDepth . evalProp' $ x
    test' (x:xs) = do
      putStrLn $ "smallchecking: " ++ show x
      res <- smallCheckM smallCheckDepth . evalProp' $ x
      case res of
        -- if no counter example was found,
        -- continue the search
        Nothing -> test' xs
        -- we found a counter example, return!
        counterExample -> pure counterExample



