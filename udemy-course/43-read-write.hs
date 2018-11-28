module ReadWrite where

getch :: IO ()
getch = do
    putStr "Type a char: "
    char <- getChar
    putStr "\nYou've typed: "
    putChar char
    putChar '\n'
