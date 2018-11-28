module Files where

import System.IO

writeFile :: IO ()
writeFile = do
    file <- openFile "test.txt" WriteMode
    hPutStr file "Writing on File!\n"
    hFlush file
    hClose file
    putStrLn "Finished file writing"
