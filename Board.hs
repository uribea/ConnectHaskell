module Board where
    --Alan Uribe
    --Germain Vargas
    mkBoard :: Int -> Int -> [[Int]]
    mkBoard n m = 
        if (n >= 0 && m >= 0)
            then take m (repeat (take n (repeat 0)))
            else[[0]]
        
    --rows :: Int->Int
    --rows i = i
    --cols :: Int->Int
    --cols i = i
    --board :: [[Int]]
    --board = mkBoard rows cols
    --mkPlayer :: Int
    --mkPlayer = 1
    --mkOpponent :: Int
    --mkOpponent = 2
    --ba = [[b | b <- bo,b == 0] | bo <- board]
    
    --isFull:: [[a]]->a
    isFull bd = if(null (concat[[b | b <- bo,b == 0] | bo <- bd]))
        then True
        else False

    isSlotOpen bd i = (bd!!(i-1)!!((numRows bd)-1) == 0)
    numSlot bd = length(bd)
    
    numRows::[[Int]]->Int
    numRows bd = length(bd!!0)
    --https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell
    transpose:: [[a]]->[[a]]
    transpose ([]:_) = []
    transpose x = (map head x) : transpose (map tail x)
    
    dropInSlot:: [[Int]]->Int->Int->[[Int]]
    dropInSlot bd i p = if(isSlotOpen (bd) i)
        then (fi++[c']++la)
        else bd
        where 
            ba = bd
            fi = frontBoard ba (i-1)
            c' = newCol (ba!!(i-1)) p
            la = backBoard ba (i-1)
            
    newCol::[Int]->Int->[Int]
    newCol col p = partCol++[p]++(take ((length col)-(length partCol+1)) (repeat 0))
        where
            partCol = [ s | s <- col, s > 0]
    
    frontBoard:: [[Int]] -> Int -> [[Int]]
    frontBoard ba i = take i ba
    
    backBoard:: [[Int]] -> Int -> [[Int]]
    backBoard ba i = drop (i+1) ba
    
    isWonBy :: [[Int]]->Int->Bool
    isWonBy bd p = isWonCols bd p 0
        
    isWonCols bd p i = 
        if (i < (numSlot bd))
            then 
                if (isWonRows bd p i 0)
                    then True
                    else isWonCols bd p (i+1)
            else
                False

    isWonRows bd p i j = 
        if (j < (numRows bd))
            then
                if(isWonCheck bd p i j 1 0 
                || isWonCheck bd p i j 0 1
                || isWonCheck bd p i j 1 1
                || isWonCheck bd p i j 1 (-1)
                )
                    then
                        True
                    else isWonRows bd p i (j+1) 
            else
                False

    isWonCheck::[[Int]]->Int->Int->Int->Int->Int->Bool
    isWonCheck bd p i j c r = ((isWonCheckLeft bd p i j c r 0) + (isWonCheckRight bd p (i+c) (j+r) c r 0)) > 3

    isWonCheckLeft bd p x y dx dy cnt= 
        if (not(dx > 0 && sx < 0) && not(dx < 0 && sx >= (numSlot bd)) 
            && not(dy > 0 && sy < 0) && not(dy < 0 && sy >= (numRows bd)) 
            && (bd!!x!!y == p))
            then
                isWonCheckLeft  bd p (x-dx) (y-dy) dx dy (cnt + 1)
            else cnt
        where
            sx = x
            sy = y

    isWonCheckRight bd p x y dx dy cnt= 
        if (not(dx > 0 && ex >= (numSlot bd)) && not(dx < 0 && ex < 0) 
            && not(dy > 0 && ey >= (numRows bd)) && not(dy < 0 && ey < 0) 
            &&  (bd!!x!!y == p))
            then
                isWonCheckRight bd p (x+dx) (y+dy) dx dy (cnt + 1)
            else cnt
        where
            ex = x
            ey = y
            
    drawTop::[[Int]]->Int->String
    drawTop bd a | a == (numSlot bd) = "\n" | otherwise = (show (a+1)) ++ " " ++ drawTop bd (a+1) ++ "--"
    
    playerToChar [] = []
    playerToChar (x:xs) =
        if x == '['
            then '\n' : playerToChar xs 
            else if x == ','
                then ' ': playerToChar xs
                else if x == ']'
                    then ' ': playerToChar xs
                    else if x == '1'
                        then 'X': playerToChar xs
                        else if x == '2'
                            then 'O': playerToChar xs
                            else '.' : playerToChar xs

    boardToStr bd = (drawTop bd 0) ++ playerToChar (drop 1( show(reverse ( transpose bd))))
    
    turn bd = ((mod (length (concat[[b | b <- bo,b == 0] | bo <- bd])) 2)+1)
