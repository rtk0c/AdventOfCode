{-# LANGUAGE OverloadedStrings #-}

import AoC
import Data.Char (isAlpha, isDigit)
import Data.Map (Map, filterWithKey, fromList, keys, lookup)
import Data.Text qualified as T

type Vertex = T.Text

parseLine :: T.Text -> (Vertex, (Vertex, Vertex))
parseLine s = (f key, (f left, f right))
  where
    (key, s') = cutOn '=' s
    (left, right) = cutOn ',' s'
    f = T.filter isAlpha

type DesertMap = Map Vertex (Vertex, Vertex)

type Direction = [Char]

parseInput :: T.Text -> (DesertMap, Direction)
parseInput s = (desertMap, direction)
  where
    s1 : s2 = T.lines s
    desertMap = fromList . map parseLine . filter (not . T.null) $ s2
    direction = T.unpack . T.strip $ s1

findPath :: DesertMap -> Direction -> Int
findPath m p0 = walk "AAA" p0
  where
    -- lookup, but assumes the key exists (i.e. assumes our input is valid)
    lookup' v = case Data.Map.lookup v m of
      Just k -> k
      Nothing -> error (T.unpack v)
    walk :: Vertex -> Direction -> Int
    walk v [] = walk v p0
    walk s (edge : ps)
      | s == "ZZZ" = 0
      | otherwise =
          case edge of
            'L' -> 1 + walk (fst $ lookup' s) ps
            'R' -> 1 + walk (snd $ lookup' s) ps

main = solveWithInput p1 p2
  where
    p1 = uncurry findPath . parseInput
    p2 = const 9