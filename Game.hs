module Play where

import Prelude hiding (getContents)
import Board

--Does IO for game functionality 
game :: Board -> IO ()
game board 
    | winner board == 1 = putStrLn X_wins
    | winner board == 2 = putStrLn O_wins
    | isDraw board == True = putStrLn draw
    | otherwise = do
        x <- readInteger (turn board)
        ok <- checkMove board x
        let board1 | ok  = makeMove board x (turn board)
                   | otherwise = board
        temp <- putStrLn ("\n" ++ (showBoard board1))
        game board1


--convert False to a String: Illegal Move!
convert :: Bool -> String
convert True = ""
convert False = "Illegal Move!\n"

--checkMove, checks move validity
checkMove :: Board -> Column -> IO Bool
checkMove b c = do
    let x = isLegalMove b c
    putStr (convert x)
    return (x)

--Prompts user input 
readInteger :: Player -> IO Int
readInteger player = do
    putStr "\nPlease enter move for "
    putStr (show player)
    putStrLn ": (0-6)"
    colPlaced <-getLine
    return (read colPlaced)

-- Displays wins or draws
draw        = "Game Over - Draw"
O_wins = "O wins!"
X_wins    = "X wins!"


play :: IO ()
play = game (newBoard (6, 7))