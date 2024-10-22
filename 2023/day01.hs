import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Data.Char (isDigit)

firstlast s = read [T.head s, T.last s] :: Integer

part1 = sum . map (firstlast . T.filter isDigit)

main = do
    input <- fmap T.lines (T.readFile "inputs/day01.txt")
    putStrLn $ "Part 1: " ++ show (part1 input)