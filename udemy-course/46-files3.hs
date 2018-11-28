module Files3 where

import System.IO

writeToFile :: IO ()
writeToFile = do
    writeFile "test.txt" "using writeFile function\n"
    putStrLn "Ok!"

readFromFile :: IO ()
readFromFile = do
    content <- readFile "test.txt"
    putStr content

appendToFile :: IO ()
appendToFile = do
    appendFile "test.txt" "a new line added to file\n"
    putStrLn "Ok!"
