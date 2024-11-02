{-# LANGUAGE OverloadedStrings #-}

import AoC (decimal, solveWithInput)
import Data.Foldable (find)
import Data.Text qualified as T

parseInput :: T.Text -> [[Int]]
parseInput = map (map decimal . T.splitOn " ") . T.lines

delta :: [Int] -> [Int]
delta lst = zipWith (-) (tail lst) lst

-- "Delta Until Constant"
-- returns a list of results of repeatedly taking the delta
duc :: [Int] -> [[Int]]
duc = takeWhile (not . all (== 0)) . iterate delta

extrapolate :: [Int] -> Int
extrapolate inp = foldr ((+) . last) 0 $ duc inp

part1 :: [[Int]] -> Int
part1 = sum . map extrapolate

main = solveWithInput p1 p2
  where
    p1 = part1 . parseInput
    p2 = const 0