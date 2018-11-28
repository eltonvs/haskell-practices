countDNA :: Char -> String -> Int
countDNA _ [] = 0
countDNA n (x:xs)
  | x == n = 1 + countDNA n xs
  | otherwise = countDNA n xs

main :: IO ()
main = do
    putStr "Type your DNA sequence: "
    dna <- getLine
    putStrLn $ "A: " ++ (show $ countDNA 'A' (show dna))
    putStrLn $ "T: " ++ (show $ countDNA 'T' (show dna))
    putStrLn $ "C: " ++ (show $ countDNA 'C' (show dna))
    putStrLn $ "G: " ++ (show $ countDNA 'G' (show dna))
