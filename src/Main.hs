module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)

hanoi :: Int -> [String]
hanoi = go "A" "B" "C"
  where
    go _ _ _ 0 = []
    go from via to n =
      go from to via (n - 1)
      ++ ["Move disc from " ++ from ++ " to " ++ to]
      ++ go via from to (n - 1)

main :: IO ()
main = do
  n <- getArgs >>= parseArgs
  let moves = hanoi n
  mapM_ putStrLn moves

parseArgs :: [String] -> IO Int
parseArgs [] = pure 4
parseArgs [s] | [(n, "")] <- reads s, n > 0 = pure n
parseArgs _ = do
  putStrLn "Invalid command line argument."
  putStrLn "Expecting a single positive integer."
  exitFailure
