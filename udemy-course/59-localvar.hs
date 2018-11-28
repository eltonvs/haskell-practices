module LocalVar where

checkNumber :: Int -> IO ()
checkNumber n =
  let myNum = 42 in if n == myNum
      then putStrLn "You found the answer!"
      else putStrLn "Ooops, not this time..."
