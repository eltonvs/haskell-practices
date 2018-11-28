main :: IO ()
main = do
    putStr "First Number: "
    n1 <- getLine
    putStr "Second Number: "
    n2 <- getLine
    putStrLn $ "Sum: " ++ (show $ read n1 + read n2)
