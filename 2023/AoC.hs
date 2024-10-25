module AoC where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)

runWithInput :: (T.Text -> IO b) -> IO b
runWithInput solution = do
  args <- getArgs
  case args of
    [filename] -> do
      file <- T.readFile filename
      solution file
    _ -> error "Required argument: <path/to/input.txt>"

solveWithInput :: (Show a1, Show a2) => (T.Text -> a1) -> (T.Text -> a2) -> IO ()
solveWithInput part1 part2 = runWithInput solve
  where
    solve file = do
      putStrLn $ "Part 1: " ++ show (part1 file)
      putStrLn $ "Part 2: " ++ show (part2 file)