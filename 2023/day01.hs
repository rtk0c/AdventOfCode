{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Data.Char (isDigit)

firstlast s = read [T.head s, T.last s] :: Integer

part1 :: [T.Text] -> Integer
part1 = sum . map (firstlast . T.filter isDigit)

-- Out of all number words, starting letters are "otfsen", and ending letters are "eolrxnt"
-- Only "eont" overlap, i.e. the order we do the replacement might matter if they involve words starting/ending with these
-- Solution: just add an extra copy of the letter to allow more replacements
wordToDigit = T.replace "one" "o1e"
    . T.replace "two" "t2o"
    . T.replace "three" "t3e"
    . T.replace "four" "4"
    . T.replace "five" "5e"
    . T.replace "six" "6"
    . T.replace "seven" "7n"
    . T.replace "eight" "e8t"
    . T.replace "nine" "9e"

part2 :: [T.Text] -> Integer
part2 = part1 . map wordToDigit

main = do
    input <- fmap T.lines (T.readFile "inputs/day01.txt")
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)