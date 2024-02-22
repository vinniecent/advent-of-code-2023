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

convertDigits :: String -> String
convertDigits "" = ""
convertDigits line@(c : xs)
    | "one"   `isPrefixOf` line = '1' : convertDigits xs
    | "two"   `isPrefixOf` line = '2' : convertDigits xs
    | "three" `isPrefixOf` line = '3' : convertDigits xs
    | "four"  `isPrefixOf` line = '4' : convertDigits xs
    | "five"  `isPrefixOf` line = '5' : convertDigits xs
    | "six"   `isPrefixOf` line = '6' : convertDigits xs
    | "seven" `isPrefixOf` line = '7' : convertDigits xs
    | "eight" `isPrefixOf` line = '8' : convertDigits xs
    | "nine"  `isPrefixOf` line = '9' : convertDigits xs
    | otherwise                 =  c  : convertDigits xs

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  let calibrationValues = map concatAndRead (map firstAndLastDigit inputLines)
 
  putStrLn "All calibration values:"
  -- mapM_ print calibrationValues

  putStrLn "Sum of all calibration values:"
  print $ sum calibrationValues

  -- print $ map replaceDigits inputLines

  let realCalibrationValues = map concatAndRead (map firstAndLastDigit ( map convertDigits inputLines ))

  putStrLn "All real calibration values:"
  -- mapM_ print realCalibrationValues

  putStrLn "Sum of all real calibration values:"
  print $ sum realCalibrationValues