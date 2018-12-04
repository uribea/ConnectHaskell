module Main where
import Board
import System.IO
import System.Exit

--Alan Uribe
--Germain Vargas
game::[[Int]]->IO ()
game board 
    | (isWonBy board 1) = putStrLn x_wins
    | (isWonBy board 2) = putStrLn o_wins
    | isFull board == True = putStrLn draw
    | otherwise = do
        putStrLn ("\n" ++ (boardToStr board))
        colPlaced <- readSlot board (turn board) 
        putStrLn(show colPlaced)
        let nextBoard1State  = (dropInSlot board colPlaced (turn board))
        putStrLn("a " ++ (boardToStr (dropInSlot board colPlaced (turn board))))
        --show nextBoard1State!!0
        putStrLn(show colPlaced)
        --putStrLn ("\n" ++ (boardToStr nextBoard1State))
        game nextBoard1State

playerChar i | i == 1 = "X" | otherwise = "O"
--Prompts user input 
--readSlot :: bd->p -> IO Int
readSlot bd p = do
    putStr "\nPlease enter move for '"
    putStr ((playerChar p))
    putStrLn "' : (1-7), -1 to quit."
    line <- getLine
    let colPlaced = reads line :: [(Int, String)] in
        if ((length colPlaced) == 0)
            then readSlot'
            else let (x, _) = head colPlaced in
            if x == (-1) 
                then 
                    exitWith (ExitSuccess)
                else if x > 0 && x <= (numSlot bd) && (isSlotOpen bd (x))
                    then return (x)
        else readSlot'
    where
        readSlot' = do
        putStrLn "Invalid input!"
        readSlot bd p

draw        = "Game Over - Draw"
x_wins = "X wins!"
o_wins    = "O wins!"


main :: IO ()
main = game (mkBoard 6 7)
