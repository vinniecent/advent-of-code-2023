module Day01 where

import Data.List (isPrefixOf)
import Paths_aoc2023 (getDataFileName)
import Data.Char (isDigit)

firstAndLastDigit :: String -> (Char, Char)
firstAndLastDigit str = case [d | d <- str, isDigit d] of
    ds -> (head ds, last ds)


concatAndRead :: (Char, Char) -> Int
concatAndRead (a, b) = read [a, b]

digitLookupMap :: [(String, String)]
digitLookupMap = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

replaceDigits :: String -> String
replaceDigits [] = []
replaceDigits str = case [ (newStr, rest) | (oldStr, newStr) <- digitLookupMap, oldStr `isPrefixOf` str,
                    let rest = drop (length oldStr) str ] of
                    ((newStr, rest):_) -> newStr ++ replaceDigits rest
                    [] -> head str : replaceDigits (tail str)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  let calibrationValues = map concatAndRead (map firstAndLastDigit inputLines)
 
  putStrLn "All calibration values:"
  mapM_ print calibrationValues

  putStrLn "Sum of all calibration values:"
  print $ sum calibrationValues

  let realCalibrationValues = map concatAndRead (map firstAndLastDigit ( map replaceDigits inputLines ))

  putStrLn "All real calibration values:"
  mapM_ print realCalibrationValues

  putStrLn "Sum of all real calibration values:"
  print $ sum realCalibrationValues