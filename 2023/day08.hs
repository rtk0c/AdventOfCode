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

parseInput :: T.Text -> (DesertMap, Path)
parseInput s = (dm, path)
  where
    s1 : s2 = T.lines s
    dm = parseMap $ filter (not . T.null) s2
    path = T.unpack $ T.strip s1

findPath :: DesertMap -> Path -> Int
findPath m p0 = walk "AAA" p0
  where
    lk v = case Data.Map.lookup v m of
      Just k -> k
      Nothing -> error (T.unpack v)
    walk :: T.Text -> Path -> Int
    walk v [] = walk v p0
    walk s (edge : ps)
      | s == "ZZZ" = 0
      | otherwise =
          case edge of
            'L' -> 1 + walk (fst $ lk s) ps
            'R' -> 1 + walk (snd $ lk s) ps

main = solveWithInput p1 p2
  where
    p1 = uncurry findPath . parseInput
    p2 = const 9