module Day04 where
import Data.List.Split (splitOn)
import Paths_aoc2023 (getDataFileName)

type Card  = ([Int], [Int]) -- (winning numbers, numbers on card)

parseCards :: String -> Card
parseCards input = (winningNumbers, numbersOnCard)
  where
    parts = splitOn " | " $ head $ tail $ splitOn ": " input
    winningNumbers = map read $ words $ head parts
    numbersOnCard = map read $ words $ last parts

takeFrom :: Int -> Int -> [a] -> [a]
takeFrom x y xs = take x (drop y xs)

day04 :: IO ()
day04 = do
  inputLines <- lines <$> (getDataFileName "day04-input.txt" >>= readFile)

  let cards = map parseCards inputLines
  -- print cards

  let winningNumbersPerCard = map (\(winningNumbers, numbersOnCard) -> filter (\n -> n `elem` winningNumbers) numbersOnCard) cards
  print winningNumbersPerCard

  let numberOfWinningNumbers = map length winningNumbersPerCard
  print numberOfWinningNumbers

  let totalPoints = sum $ map ((2 ^) . (+ (-1))) $ filter (/= 0) $ numberOfWinningNumbers
  print totalPoints

  let expandedWinningScratchcards = sum $ map (+1) $ foldr (\(winningCount) acc -> winningCount + sum (take winningCount acc) : acc) [] numberOfWinningNumbers
  print expandedWinningScratchcards
