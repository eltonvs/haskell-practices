module TicTacToe where

import Control.Exception
import System.IO
import System.IO.Error
import System.Process

-- Data types definition
type Name = String
type Score = Int
type Players = [Player]
type PlayerTime = Int
type Board = [Char]

data Player = Player Name Score
  deriving (Show, Read)

getString :: String -> IO String
getString str = do
  putStr str
  input <- getLine
  return input

-- Function to start application
start :: IO ()
start = do
  {catch readDataFile treatErr;}
    where
      readDataFile = do
        {
          file <- openFile "game.dat" ReadMode;
          fData <- hGetLine file;
          hClose file;
          menu $ read fData;
          return ()
        }
      treatErr err = if isDoesNotExistError err
        then do
        {
          file <- openFile "game.dat" WriteMode;
          hPutStrLn file "[]";
          hClose file;
          menu [];
          return ();
        }
        else ioError err

-- Show Menu
menu :: Players -> IO Players
menu data' = do
  system "clear";
  putStrLn "--- Tic Tac Toe ---"
  putStrLn " 1 - Register Player"
  putStrLn " 2 - Play Game"
  putStrLn " 3 - Show Ranking"
  putStrLn " 0 - Quit"
  putStr ">>> "
  op <- getChar
  getChar
  executeOption data' op

executeOption :: Players -> Char -> IO Players
executeOption data' '0' = do
  putStrLn "\nBye! See you soon!"
  return data'
executeOption data' '1' = registerPlayer data'
executeOption data' '2' = prepareGame data'
executeOption data' '3' = showRanking data'
executeOption data' _ = do
  putStrLn "\nInvalid Option! Try again..."
  putStr "\nPress any key to return to menu..."
  getChar
  menu data'

registerPlayer :: Players -> IO Players
registerPlayer data' = do
  pName <- getString "\n>>> Your username: "
  if hasPlayer data' pName
    then do
      putStrLn "This username was already registered. Please try again..."
      registerPlayer data'
      -- menu data'
    else do
      file <- openFile "game.dat" WriteMode
      let newData = (Player pName 0):data'
      hPutStrLn file $ show newData
      hClose file
      putStrLn $ "User \"" ++ pName ++ "\" was successfully registered!"
      putStr "\nPress any key to continue..."
      getChar
      menu newData

  return data'

hasPlayer :: Players -> Name -> Bool
hasPlayer [] _ = False
hasPlayer ((Player pn _):xs) n = if n == pn then True else hasPlayer xs n

prepareGame :: Players -> IO Players
prepareGame data' = do
  p1 <- askForPlayer data' 1
  p2 <- askForPlayer data' 2
  newGame data' p1 p2

askForPlayer :: Players -> Int -> IO Name
askForPlayer data' n = do
  p <- getString $ "\n>>> Player " ++ show n ++ " username: "
  if hasPlayer data' p
    then return p
    else do
      putStrLn "This player does not exists! Please, try again..."
      askForPlayer data' n

newGame :: Players -> Name -> Name -> IO Players
newGame data' p1 p2 = do
  putStrLn $ "Starting a new Game: \"" ++ p1 ++ "\" vs \"" ++ p2 ++ "\""
  putStrLn $ " " ++ p1 ++ " -> 'X'"
  putStrLn $ " " ++ p2 ++ " -> 'O'"
  runGame data' newBoard p1 p2 0

newBoard :: Board
newBoard = ['1'..'9']

runGame :: Players -> Board -> Name -> Name -> PlayerTime -> IO Players
runGame data' board p1 p2 t = do
  return data'

printBoard :: Board -> IO ()
printBoard board = do
  printBoard' board 0
  printBoard' board 1
  printBoard' board 2 where
    pad = replicate 5 ' '
    printBoard' :: Board -> Int -> IO ()
    printBoard' board row = do
      putStrLn $ pad ++ [board !! (3 * row)] ++ " | " ++
        [board !! (3 * row + 1)] ++ " | " ++ [board !! (3 * row + 2)]
      if row < 2 then putStrLn $ pad ++ replicate 9 '-' else do return ()

showRanking data' = menu data'
