module Main where
 import Board
    --Alan Uribe
    game :: Board -> IO ()
    game board 
    | isWonBy board 1 = putStrLn x_wins
    | isWonBy board 2 = putStrLn o_wins
    | isFull board == True = putStrLn draw
    | otherwise = do
        colPlaced <- readSlot (turn board)
        legalMove <- checkMove board colPlaced
        let nextBoard1State | legalMove  = dropInSlot board colPlaced (turn board)
                   | otherwise = board
        putStrLn ("\n" ++ (boardToStr nextBoard1State))
        game nextBoard1State

    --Prompts user input 
    readSlot :: bd->p -> IO Int
    readSlot bd p = do
        putStr "\nPlease enter move for "
        putStr (if p == 1 then show 'X' else 'O')
        putStrLn ": (1-7), -1 to quit."
        colPlaced <-getLine
        if ((length colPlaced) == 0)
            then readSlot'
            else if(colPlaced == "-1")
            then do
                putStrLn "GoodBye!"
                exitWith (ExitSuccess)
            else do
                if(isLetter (colPlaced !! 0))
                    then do 
                        putStrLn "Invalid input!"
                        readSlot player
                    else do
                        return ((read colPlaced)-1)

draw        = "Game Over - Draw"
x_wins = "X wins!"
o_wins    = "O wins!"


main :: IO ()
main = game (mkBoard (6,7))