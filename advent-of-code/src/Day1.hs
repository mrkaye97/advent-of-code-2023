main = do
    s <- readFile "src/data/day1.txt"
    doSomethingWith s

doSomethingWith :: String -> IO ()
doSomethingWith str = putStrLn str