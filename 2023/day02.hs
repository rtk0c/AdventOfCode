{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T

decimal :: T.Text -> Int
decimal = fst . either error id . T.decimal

map2 f g (x, y) = (f x, g y)

map3 f g h (x, y, z) = (f x, g y, h z)

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

-- Same as Data.Text.breakOn on a char, but removes the separator char
cutOn :: Char -> T.Text -> (T.Text, T.Text)
cutOn c s =
  let (front, back) = T.breakOn (T.singleton c) s
   in (front, T.tail back)

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

mainWithFile f = do
  input <- fmap parseInput (T.readFile f)
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 (map snd input))

main = mainWithFile "inputs/day02.txt"