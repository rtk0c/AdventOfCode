{-# LANGUAGE OverloadedStrings #-}

import AoC (decimal, solveWithInput)
import Data.Text qualified as T

mapInd :: (Int -> a -> c) -> [a] -> [c]
mapInd f = zipWith f [0 ..]

parseInput :: T.Text -> [(Int, Int)]
parseInput = concat . mapInd parseLine . T.lines
  where
    parseLine :: Int -> T.Text -> [(Int, Int)]
    parseLine y l = map parseCell $ filter discard $ zip [0 ..] (T.unpack l)
      where
        discard (_, c) = c == '#'
        parseCell (x, _) = (x, y)