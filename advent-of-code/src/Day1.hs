module Day1 where

import Data.List.Split (splitOn)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List (find)

main :: IO ()
main = do
    -- s <- readFile "src/data/day1.txt"
    -- doSomethingWith s
    print $ findFirstLastPair "xy24"

-- doSomethingWith :: String -> IO ()
-- doSomethingWith str = print $ splitInputByLine str

-- splitInputByLine x = splitOn "\n" x

findFirstLastPair str = x ++ y
    where
        x = case findFirstDigit str of
            Just z -> [z]
            _ -> ""
        y = case findLastDigit str of
            Just z -> [z]
            _ -> ""

findFirstDigit str = find isDigit str
findLastDigit str = find isDigit $ reverse str

isDigit :: Char -> Bool
isDigit x = case readMaybe [x] :: Maybe Int of
    Just y | y `elem` [0 .. 9]  -> True
    _                           -> False
