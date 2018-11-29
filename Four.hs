module Four where
    --mkBoard :: Int -> Int -> [[Int]]
    mkBoard n m = 
        if (n >= 0 && m >= 0)
            then take n (repeat (take m [1,2,3,4,5,6,7]))
            else[[0]]    
    rows :: Int
    rows = 6
    cols :: Int
    cols = 7
    board = mkBoard rows cols
    mkPlayer :: Int
    mkPlayer = 1
    mkOpponent :: Int
    mkOpponent = 2
    ba = [[b | b <- bo,b == 0] | bo <- board]
    
    --isFull:: [[a]]->a
    isFull bd = if(null (concat[[b | b <- bo,b == 0] | bo <- bd]))
        then True
        else False

    isSlotOpen bd i = if(not(bd!!(length[bd]-1)!!i-1 == 0))
        then True
        else False
    
    numSlot bd = length(bd!!0)
    --https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell
    transpose:: [[a]]->[[a]]
    transpose ([]:_) = []
    transpose x = (map head x) : transpose (map tail x)

    dropInSlot bd i p = if(isSlotOpen bd i)
        then transpose bd
        else bd
