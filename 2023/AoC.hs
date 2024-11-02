module AoC where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import System.Environment (getArgs)

-- Same as Data.Text.breakOn on a char, but removes the separator char
cutOn :: Char -> T.Text -> (T.Text, T.Text)
cutOn c s =
  let (front, back) = T.breakOn (T.singleton c) s
   in (front, T.tail back)

decimal :: T.Text -> Int
decimal = fst . either error id . T.signed T.decimal

intersectSorted :: (Ord a) => [a] -> [a] -> [a]
intersectSorted _ [] = []
intersectSorted [] _ = []
intersectSorted xs@(x : xrest) ys@(y : yrest)
  | x > y = intersectSorted xs yrest
  | x < y = intersectSorted xrest ys
  | otherwise = x : intersectSorted xrest yrest

map2 f g (x, y) = (f x, g y)
map2a f (x, y) = (f x, f y)

map3 f g h (x, y, z) = (f x, g y, h z)
map3a f (x, y, z) = (f x, f y, f z)

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