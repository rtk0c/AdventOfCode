{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Data.Char (isDigit)

firstlast s = read [T.head s, T.last s] :: Integer

part1 :: [T.Text] -> Integer
part1 = sum . map (firstlast . T.filter isDigit)

wordToDigit = T.replace "one" "1"
    . T.replace "two" "2"
    . T.replace "three" "3"
    . T.replace "four" "4"
    . T.replace "five" "5"
    . T.replace "six" "6"
    . T.replace "seven" "7"
    . T.replace "eight" "8"
    . T.replace "nine" "9"

part2 :: [T.Text] -> Integer
part2 = part1 . map wordToDigit

main = do
    input <- fmap T.lines (T.readFile "inputs/day01.txt")
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)