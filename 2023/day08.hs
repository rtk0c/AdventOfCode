{-# LANGUAGE OverloadedStrings #-}

import AoC
import Data.Char (isAlpha, isDigit)
import Data.Map (Map, fromList, lookup)
import Data.Text qualified as T

parseLine :: T.Text -> (T.Text, (T.Text, T.Text))
parseLine s = (f key, (f left, f right))
  where
    (key, s') = cutOn '=' s
    (left, right) = cutOn ',' s'
    f = T.filter isAlpha

type DesertMap = Map T.Text (T.Text, T.Text)

type Path = [Char]

parseMap :: [T.Text] -> DesertMap
parseMap = fromList . map parseLine

textToStr :: T.Text -> String
textToStr = reverse . T.foldl (flip (:)) []

parseInput :: T.Text -> (DesertMap, Path)
parseInput s = (dm, path)
  where
    s1 : s2 = T.lines s
    dm = parseMap $ filter (not . T.null) s2
    path = textToStr $ T.strip s1

-- part1 :: DesertMap -> Path -> Int
part1 m p0 = walkPath "AAA" p0
  where
    lk v = case Data.Map.lookup v m of
      Just k -> k
      Nothing -> error (textToStr v)
    start = lk "AAA"
    -- walkPath :: T.Text -> Path -> Int
    walkPath s [] = walkPath s p0
    walkPath s (edge : ps)
      | s == "ZZZ" = [s]
      | otherwise =
          case edge of
            'L' -> s : walkPath (fst $ lk s) ps
            'R' -> s : walkPath (snd $ lk s) ps

main = solveWithInput p1 p2
  where
    p1 = uncurry part1 . parseInput
    p2 = const 9