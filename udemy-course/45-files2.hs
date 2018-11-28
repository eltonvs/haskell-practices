module Files2 where

import System.IO

readFile :: IO ()
readFile = do
    file <- openFile "test.txt" ReadMode
    content <- hGetContents file
    putStrLn content
    hClose file
