import AoC (cutOn, decimal, solveWithInput)
import Data.Bifunctor (bimap)
import Data.List (transpose)
import Data.Text qualified as T
import Data.Char (isDigit)

-- Let t be the total time allowed in race (constant)
-- Let tc be the time used to charge the race (parameter)
--
-- Let v0 be the starting accelaration
--     v0 = tc * 1mm/ms^2
-- Let x be the total distance traveled
--     x = (t - tc) * v0
--       = (t - tc) * tc
--       = t*tc - tc^2
-- Find values of tc such that x > K, where K is an arbitary reKord time (constant)
--
-- 1. t*tc - tc^2 > K
-- 2. Find values of tc such that LHS = RHS
--       -tc^2 + t*tc - K = 0               (rearrange)
--    => tc = (-t ± sqrt(t^2 - 4*K)) / -2   (quadratic formula)
--    Let the two solutions be t1 and t2
-- 3. LHS is increasing in (-∞, t/2]        (critical point, alternatively vertex of a parabola)
--    and decreasing in [t/2, +∞)
-- 4. Conclusion: { n | n ∈ ℕ ∧ (t1 < n < t2) }
--    Note that it must be an open interval (t1,t2) here because getting the same time as the past record is not breaking it

solveQuadratic :: Double -> Double -> Double -> (Double, Double)
solveQuadratic a b c = (vx - dist, vx + dist)
  where
    vx = (-b) / (2 * a)
    dist = sqrt (vx ^ 2 - c / a)

nextInt x = floor x + 1

prevInt x = ceiling x - 1

solveChargeTimes :: Double -> Double -> Int
solveChargeTimes timeLimit record = prevInt t2 - nextInt t1 + 1
  where
    (t1, t2) = solveQuadratic (-1) timeLimit (-record)

-- Just solveChargeTimes but pass the parameters differently
solveTupII :: (Int, Int) -> Int
solveTupII = uncurry solveChargeTimes . bimap fromIntegral fromIntegral

stripTo :: Char -> T.Text -> T.Text
stripTo c = snd . cutOn c

parsePart1 :: T.Text -> [(Int, Int)]
parsePart1 s = zip timeLimits records
  where
    parseLine = map decimal . T.words . stripTo ':'
    [timeLimits, records] = map parseLine . T.lines $ s

part1 :: [(Int, Int)] -> Int
part1 = product . map solveTupII

parsePart2 :: T.Text -> (Int, Int)
parsePart2 s = (timeLimit, record)
  where
    parseLine = decimal . T.filter isDigit . stripTo ':'
    [timeLimit, record] = map parseLine . T.lines $ s

part2 :: (Int, Int) -> Int
part2 = solveTupII

main = solveWithInput p1 p2
  where
    p1 = part1 . parsePart1
    p2 = part2 . parsePart2