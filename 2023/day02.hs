{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T

dropBefore c = T.tail . snd . T.breakOn c

parse :: T.Text -> Int
parse = fst . either error id . T.decimal

parseElm str = case T.split (== ' ') str of
  n : "red" : _ -> (parse n, 0, 0)
  n : "green" : _ -> (0, parse n, 0)
  n : "blue" : _ -> (0, 0, parse n)
  _ -> (0, 0, 0)

type Sample = (Int, Int, Int)

merge :: Sample -> Sample -> Sample
merge (a, b, c) (x, y, z) = (a + x, b + y, c + z)

parseSample :: T.Text -> Sample
parseSample =
  foldl1 merge
    . map (parseElm . T.strip)
    . T.split (== ',')

parseLine :: T.Text -> [Sample]
parseLine = map (parseSample . T.strip) . T.split (== ';')

-- https://stackoverflow.com/a/16192050
mapInd :: (a -> b) -> [a] -> [(Int, b)]
mapInd f = zipWith (\i a -> (i, f a)) [1 ..]

parseInput :: T.Text -> [(Int, [Sample])]
parseInput = mapInd parseLine . map T.strip . map (dropBefore ":") . T.lines

illegalSample (r, g, b) = r > 12 || g > 13 || b > 14

illegalGame = any illegalSample

part1 = sum . map fst . filter (illegalGame . snd)

part2 input = 0

mainWithFile f = do
  input <- fmap parseInput (T.readFile f)
  print input
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

main = mainWithFile "inputs/day02.txt"