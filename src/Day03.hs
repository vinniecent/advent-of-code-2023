module Day03 where
import Data.Char

import Paths_aoc2023 (getDataFileName)

data Number = Number { value :: Int, numRow :: Int, numSpan :: (Int, Int) } deriving (Show)
data Symbol = Symbol { char :: Char, symRow :: Int, symCol :: Int } deriving (Show)

isSymbol :: Char -> Bool
isSymbol c = not (c == '.' || isDigit c)

extractNumbers :: String -> [Number]
extractNumbers input = traverseInput input 0 0
  where
    traverseInput [] _ _ = []
    traverseInput (c:cs) pos row
      | isDigit c = let (num, rest) = Prelude.span isDigit (c:cs)
                        numValue = read num :: Int
                        numEndPos = pos + (length num)
                    in Number numValue row (pos, numEndPos - 1) : traverseInput rest numEndPos row
      | c == '\n' = traverseInput cs 0 (row + 1)
      | otherwise = traverseInput cs (pos + 1) row

extractSymbols :: String -> [Symbol]
extractSymbols input = traverseInput input 0 0
  where
    traverseInput [] _ _ = []
    traverseInput (c:cs) col row
      | c == '\n' = traverseInput cs 0 (row + 1)
      | Day03.isSymbol c = Symbol c row col : traverseInput cs (col + 1) row
      | otherwise = traverseInput cs (col + 1) row

-- Check if a Number is a neighbor of a Symbol
isNeighbour :: Number -> Symbol -> Bool
isNeighbour (Number _ numRow' (numStart, numEnd)) (Symbol _ symRow' symPos) =
    (abs (numRow' - symRow') <= 1) && ((numStart-1 <= symPos) && symPos <= numEnd+1)


day03 :: IO ()
day03 = do
  inputLines <- lines <$> (getDataFileName "day03-input.txt" >>= readFile)
  let numbers = extractNumbers $ unlines inputLines
  -- print numbers

  let symbols = extractSymbols $ unlines inputLines
  -- print symbols
  
  let gears = filter (\s -> char s == '*') symbols
  -- print gears

  let numbersAdjacentToSymbols = filter (\(num, sym) -> isNeighbour num sym) [(num, sym) | num <- numbers, sym <- symbols]
  -- print numbersAdjacentToSymbols

  let sumPartNumbers = sum $ map (value . fst) numbersAdjacentToSymbols
  print sumPartNumbers

  let gearsWithNumbers = map (\g -> (g, filter (\n -> isNeighbour n g) numbers)) gears
  -- print gearsWithNumbers

  let gearsWithTwoNumbers = filter (\(_, ns) -> length ns == 2) gearsWithNumbers
  -- print gearsWithTwoNumbers

  -- multiply the values of the numbers adjacent to the gears
  let gearsRatios = map (\(g, ns) -> (product $ map value ns)) gearsWithTwoNumbers
  -- print gearsRatios

  let sumOfGearRatios = sum gearsRatios
  print sumOfGearRatios