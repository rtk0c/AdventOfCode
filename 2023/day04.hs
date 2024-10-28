{-# LANGUAGE OverloadedStrings #-}

import AoC (cutOn, decimal, map3, solveWithInput)
import Data.Char (isDigit)
import Data.List (sort)
import Data.Text qualified as T
import Data.Text.Read qualified as T

type Pile = (Int, [Int], [Int])

sortPile :: Pile -> Pile
sortPile = map3 id sort sort

parsePile :: T.Text -> Pile
parsePile s =
  let (p1, rest) = cutOn ':' s
      (p2, p3) = cutOn '|' rest
   in (parseIndex p1, parseIntList p2, parseIntList p3)
  where
    parseIntList = map (decimal . T.strip) . filter (not . T.null) . T.splitOn " " . T.strip
    parseIndex = decimal . T.filter isDigit

parseInput :: T.Text -> [Pile]
parseInput = map parsePile . T.lines

intersectSorted :: (Ord a) => [a] -> [a] -> [a]
intersectSorted _ [] = []
intersectSorted [] _ = []
intersectSorted xs@(x : xrest) ys@(y : yrest)
  | x > y = intersectSorted xs yrest
  | x < y = intersectSorted xrest ys
  | otherwise = x : intersectSorted xrest yrest

winningCount :: Pile -> Int
winningCount (_, front, back) = length $ intersectSorted front back

score :: Int -> Int
score n
  | n > 0 = 2 ^ (n - 1)
  | otherwise = 0

part1 :: [Pile] -> Int
part1 piles = sum $ map (score . winningCount . sortPile) piles

main = solveWithInput p1 p2
  where
    p1 = part1 . parseInput
    p2 = const 0