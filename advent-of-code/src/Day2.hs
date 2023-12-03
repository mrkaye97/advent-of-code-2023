{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.List (find, isInfixOf, elemIndex)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.Text (pack, replace, unpack)
import qualified Data.Text.IO as T

main :: IO ()
main = do
    s <- readFile "src/data/day2.txt"
    print $ lines s
    print $ Color Blue 10
    print $ Draw (Just 10) (Just 12) (Just 14)
    print $ map parseGame (lines s)

data ColorName = Blue | Green | Red
    deriving (Show)

data Color = Color
  { colorName :: ColorName
  , colorLimit :: Int
  }
  deriving (Show)

data Draw = Draw
    {
        blue :: Maybe Int,
        red :: Maybe Int,
        green :: Maybe Int
    }
    deriving (Show)

data Game = Game
    {
        gameId :: Int,
        draws :: [Draw]
    }
    deriving (Show)

parseGame :: String -> Game
parseGame str = Game gameId draws
  where
    parts = splitOn ": " str
    gameId = read (unpack $ replace "Game " "" (pack (head parts)))
    draws = map getColorCount (tail parts)

getColorCount :: String -> Draw
getColorCount str = Draw blue red green
    where
        blue = parseComponent "blue" str
        green = parseComponent "green" str
        red = parseComponent "red" str

parseComponent :: String -> String -> Maybe Int
parseComponent componentName str =
    case find (componentName `isInfixOf`) (splitOn ", " str) of
        Just component -> Just $ read $ head $ splitOn " " component
        Nothing -> Nothing
