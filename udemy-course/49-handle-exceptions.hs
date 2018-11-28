module ExceptionHandling where

import Control.Exception
import System.IO.Error

read_file :: IO ()
read_file = do
    {catch readfile catchErr;}
        where
            readfile = do
            {
                content <- readFile "myFile.txt";
                return $ read content;
            }
            catchErr err = if isDoesNotExistError err
                then do
                {
                    putStrLn "File not found";
                    -- treat exception
                }
                else ioError err
