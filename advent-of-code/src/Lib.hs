module Lib
    ( someFunc, func2
    ) where

someFunc :: IO ()
someFunc = putStrLn "Foo Bar Baz"

func2 :: IO ()
func2 = putStrLn "Baz Qux"