{-# LANGUAGE OverloadedStrings #-}

import AoC (cutOn, decimal, solveWithInput)
import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Text.Read qualified as T

parseElm str = case T.split (== ' ') str of
  n : "red" : _ -> (decimal n, 0, 0)
  n : "green" : _ -> (0, decimal n, 0)
  n : "blue" : _ -> (0, 0, decimal n)
  _ -> (0, 0, 0)

type Sample = (Int, Int, Int)

merge :: Sample -> Sample -> Sample
merge (a, b, c) (x, y, z) = (a + x, b + y, c + z)

-- Turn "1 red, 2 blue, 3 green" into (1, 3, 2), aka Sample
parseSample :: T.Text -> Sample
parseSample = foldl1 merge . map (parseElm . T.strip) . T.split (== ',')

-- Turn ';' separated samples into [Sample]
parseSamples :: T.Text -> [Sample]
parseSamples = map (parseSample . T.strip) . T.split (== ';')

-- Turn "Game NNN" into Int NNN
parseIndex :: T.Text -> Int
parseIndex = decimal . T.filter isDigit

parseInput :: T.Text -> [(Int, [Sample])]
parseInput = map lf . T.lines
  where
    lf line =
      let (game, samples) = cutOn ':' line
       in (parseIndex game, parseSamples samples)

legalSample (r, g, b) = r <= 12 && g <= 13 && b <= 14

legalGame = all legalSample

part1 :: [(Int, [Sample])] -> Int
part1 = sum . map fst . filter (legalGame . snd)

transpose :: [(a, a, a)] -> ([a], [a], [a])
transpose ((r, g, b) : xs) = (r : rs, g : gs, b : bs)
  where
    (rs, gs, bs) = transpose xs
transpose [] = ([], [], [])

power :: (Int, Int, Int) -> Int
power (r, g, b) = r * g * b

minCubes :: [Sample] -> (Int, Int, Int)
minCubes game =
  let (reds, greens, blues) = transpose game
   in (maximum reds, maximum greens, maximum blues)

part2 :: [[Sample]] -> Int
part2 = sum . map (power . minCubes)

main = solveWithInput p1 p2
  where
    p1 = part1 . parseInput
    p2 = part2 . map snd . parseInput