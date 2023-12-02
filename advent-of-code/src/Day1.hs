module Day1 where

import Data.List (find, isInfixOf)
import Data.Maybe
import Text.Read (readMaybe)

main :: IO ()
main = do
    s <- readFile "src/data/day1.txt"
    let calibrationValues = catMaybes $ map findFirstLastPair (lines s)
    print $ sum calibrationValues
    print $ findFirstSpelledOutDigit "21onetwo4"

findFirstLastPair str = readMaybe (x ++ y)
  where
    x = case findFirstDigit str of
        Just z -> [z]
        _ -> ""
    y = case findLastDigit str of
        Just z -> [z]
        _ -> ""

findFirstDigit str = find isDigit str

findLastDigit str = find isDigit $ reverse str

isDigit x = case readMaybe [x] :: Maybe Int of
    Just y | y `elem` [0 .. 9] -> True
    _ -> False

spelledOutDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

findFirstSpelledOutDigit search = findSpelledOutDigit search spelledOutDigits

findSpelledOutDigit search digits = find (`isInfixOf` search) digits
