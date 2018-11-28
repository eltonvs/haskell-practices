import System.Random

riddle number = do
    putStrLn ""
    putStr "Type a number between 1 and 50: "
    choice <- getLine
    if read choice > number
        then do
            putStrLn "Ohh your choice was higher. Try again!"
            riddle number
        else if read choice < number
            then do
                putStrLn "Too lower... I'll give you another chance:"
                riddle number
            else putStrLn "Yeah! You got it!"

main :: IO ()
main = do
    num <- randomRIO (0::Int, 50)
    riddle num
