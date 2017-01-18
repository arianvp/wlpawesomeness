{-#LANGUAGE MonadComprehensions #-}
module Verify where


import Language
import Logic
import Wlp
import ProgramPath
import Eval
import Unshadow
import Test.SmallCheck
import Test.SmallCheck.Drivers



verify :: Int -> Int -> [Statement] -> IO ()
verify smallCheckDepth pathsDepth program = do
  putStrLn "I am now testing your program:"
  print program
  putStrLn "with the following parameters:"
  putStrLn $ "\t smallCheckDepth = " ++ show smallCheckDepth
  putStrLn $ "\t pathsDepth = " ++ show pathsDepth
  putStrLn "Hold on your butts! this might take a while"
  maybeFailure <- test' (verify' smallCheckDepth pathsDepth program)
  case maybeFailure of
    Just failure -> putStrLn $ ppFailure failure
    Nothing -> putStrLn "Completed!"

  where
    test' [] = error "there was nothing to test at this depth"
    test' (x:[]) = smallCheckM smallCheckDepth x
    test' (x:xs) = do
      res <- smallCheckM smallCheckDepth x
      case res of
        -- if no counter example was found,
        -- continue the search
        Nothing -> test' xs
        -- we found a counter example, return!
        counterExample -> pure counterExample

verify' :: Monad m => Int -> Int -> [Statement] -> [Property m]
verify' smDepth pathsDepth
  = map (changeDepth (const smDepth) . evalProp' . sortedPrenex . flip wlp (BoolVal True))
  . paths pathsDepth
  . unshadow


