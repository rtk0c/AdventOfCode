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

-- L or R
type Step = Char

-- the ex. LLRRRLRRL string provided in input
type Directions = [Step]

parseInput :: T.Text -> (DesertMap, Directions)
parseInput s = (desertMap, direction)
  where
    s1 : s2 = T.lines s
    desertMap = fromList . map parseLine . filter (not . T.null) $ s2
    direction = T.unpack . T.strip $ s1

followDirections :: DesertMap -> Directions -> [Vertex]
followDirections m p0 = scanl nextVert "AAA" (cycle p0)
  where
    -- lookup, but assumes the key exists (i.e. assumes our input is valid)
    lookup' v = case Data.Map.lookup v m of
      Just k -> k
      Nothing -> error (T.unpack v)
    nextVert :: Vertex -> Step -> Vertex
    nextVert v edge =
      case edge of
        'L' -> fst $ lookup' v
        'R' -> snd $ lookup' v

part1 :: (DesertMap, Directions) -> Int
part1 (desertMap, directions) = length path
  where
    -- List of every vertex we travel through, until we get to ZZZ
    path = takeWhile (/= "ZZZ") (followDirections desertMap directions)

main = solveWithInput p1 p2
  where
    p1 = part1 . parseInput
    p2 = const 9