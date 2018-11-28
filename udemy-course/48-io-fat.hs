fat :: Int -> Int
fat 0 = 1
fat n = n * fat (n - 1)

main :: IO ()
main = do
    putStr "Number: "
    nb <- getLine
    putStrLn $ "Result = " ++ (show $ fat (read nb))
