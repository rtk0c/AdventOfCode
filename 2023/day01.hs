{-# LANGUAGE OverloadedStrings #-}

import AoC (solveWithInput)
import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Text.Read qualified as T

firstlast s = read [T.head s, T.last s] :: Int

part1 :: [T.Text] -> Int
part1 = sum . map (firstlast . T.filter isDigit)

-- Out of all number words, starting letters are "otfsen", and ending letters are "eolrxnt"
-- Only "eont" overlap, i.e. the order we do the replacement might matter if they involve words starting/ending with these
-- Solution: just add an extra copy of the letter to allow more replacements
wordToDigit =
  T.replace "one" "o1e"
    . T.replace "two" "t2o"
    . T.replace "three" "t3e"
    . T.replace "four" "4"
    . T.replace "five" "5e"
    . T.replace "six" "6"
    . T.replace "seven" "7n"
    . T.replace "eight" "e8t"
    . T.replace "nine" "9e"

part2 :: [T.Text] -> Int
part2 = part1 . map wordToDigit

main = solveWithInput p1 p2
  where
    p1 = part1 . T.lines
    p2 = part2 . T.lines