module Day1 where

import Data.List.Split (splitOn)
import Data.Maybe
import Data.Text (find)
import Text.Read (readMaybe)

main :: IO ()
main = do
    -- s <- readFile "src/data/day1.txt"
    -- doSomethingWith s
    print $ isDigit "9"

doSomethingWith :: String -> IO ()
doSomethingWith str = print $ splitInputByLine str

splitInputByLine x = splitOn "\n" x

findFirstDigit :: [Char] -> Int
findFirstDigit str = find isDigit str

isDigit x = case readMaybe x :: Maybe Int of
    Just y | y `elem` [0 .. 9]  -> True
    _                           -> False
