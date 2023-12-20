module Day02 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Split (splitOn)
import Data.List (find)

data Game = Game { id :: Int, maxBlue :: Int, maxRed :: Int, maxGreen :: Int } deriving (Show)

findColor :: String -> [(Int, String)] -> Maybe (Int, String)
findColor x = find (\ (_, s) -> s == x)

parseColor :: String -> (Int, String)
parseColor s = (read (head parts) :: Int, last parts)
  where
    parts = words s

parseRound :: String -> (Int, Int, Int)
parseRound s = (blue, red, green)
  where
    parts = splitOn ", " s
    colors = map parseColor parts
    blue = maybe 0 fst (findColor "blue" colors)
    red = maybe 0 fst (findColor "red" colors)
    green = maybe 0 fst (findColor "green" colors)


parseGame :: String -> Game
parseGame input = Game gameId maxBlueRounds maxRedRounds maxGreenRounds
  where
    parts = splitOn ": " input
    gameId = read $ last $ words $ head parts :: Int
    rounds = map parseRound (splitOn "; " (last parts))
    maxBlueRounds = maximum $ map (\(blue, _, _) -> blue) rounds
    maxRedRounds = maximum $ map (\(_, red, _) -> red) rounds
    maxGreenRounds = maximum $ map (\(_, _, green) -> green) rounds

power :: Game -> Int
power (Game _ blue red green) = blue * red * green

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  
  let games = map parseGame inputLines

  let possibleGames = filter (\(Game _ blue red green) -> blue <= 14 && red <= 12 && green <= 13) games

  let sumOfIds = sum $ map (\(Game num _ _ _) -> num) possibleGames

  print sumOfIds

  let powersOfGames = map power games

  let sumOfPowers = sum powersOfGames

  print sumOfPowers
