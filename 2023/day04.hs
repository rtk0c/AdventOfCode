{-# LANGUAGE OverloadedStrings #-}

import AoC (cutOn, decimal, intersectSorted, solveWithInput)
import Data.Char (isDigit)
import Data.List (sort)
import Data.Text qualified as T
import Data.Text.Read qualified as T

type Card = (Int, [Int], [Int])

sortCardNums :: Card -> Card
sortCardNums (ind, winNums, myNums) = (ind, sort winNums, sort myNums)

parseCard :: T.Text -> Card
parseCard s =
  let (p1, rest) = cutOn ':' s
      (p2, p3) = cutOn '|' rest
   in (parseIndex p1, parseIntList p2, parseIntList p3)
  where
    parseIntList = map (decimal . T.strip) . filter (not . T.null) . T.splitOn " " . T.strip
    parseIndex = decimal . T.filter isDigit

parseInput :: T.Text -> [Card]
parseInput = map parseCard . T.lines

winningCount :: Card -> Int
winningCount (_, front, back) = length $ intersectSorted front back

score :: Int -> Int
score n
  | n > 0 = 2 ^ (n - 1)
  | otherwise = 0

part1 :: [Card] -> Int
part1 = sum . map (score . winningCount . sortCardNums)

growPile (k, c) x = (k + x, c)

-- Play each scratchcard pile in the list. For every `n` wins from a pile of `k` scratchcard, duplicate the next `n` scratchcards `k` times.
playCards :: [(Int, Card)] -> [Int]
playCards [] = []
playCards ((k, card) : ps) =
  k : playCards psRewarded
  where
    rewards = winningCount card
    psRewarded = zipWith growPile ps (replicate rewards k ++ repeat 0)

makeCardPiles :: Int -> [Card] -> [(Int, Card)]
makeCardPiles n = map (n,)

part2 :: [Card] -> Int
part2 = sum . playCards . makeCardPiles 1

main = solveWithInput p1 p2
  where
    p1 = part1 . map sortCardNums . parseInput
    p2 = part2 . map sortCardNums . parseInput