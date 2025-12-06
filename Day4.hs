
main :: IO ()
main = do 
    contents <- readFile "Day4.txt"
    let paperGrid = fmap convertToArray $ lines contents 
        coords = createCoords (length $ paperGrid !! 1) (length paperGrid)
        flatCoords = concat coords
        flatCoordsToCheck = filter (\x -> paperGrid !! (fst x) !! (snd x)) flatCoords

    print paperGrid
    print flatCoords
    print flatCoordsToCheck
    print $ fmap (\x -> getNeighboursCount (fst x) (snd x) paperGrid) flatCoordsToCheck 
    print $ length $ filter (<4) $ fmap (\x -> getNeighboursCount (fst x) (snd x) paperGrid) flatCoordsToCheck
    print $ countAmount paperGrid


convertToArray :: String -> [Bool]
convertToArray text = fmap (\x -> if x == '@' then True else False) text

createCoords :: Int -> Int -> [[(Int, Int)]]
createCoords x y = fmap 
    (\yVal -> zip (repeat yVal) [0..x-1]) [0..y-1]
    
getValueAtCoord :: Int -> Int -> [[Bool]] -> Int
getValueAtCoord x y paperGrid = if (paperGrid !! y !! x) then 1 else 0

getNeighboursCount :: Int -> Int -> [[Bool]] -> Int
getNeighboursCount x y paperGrid = 
    let coords = generateCoordList x y paperGrid
    in  (sum $ fmap (\a -> if (paperGrid !! (snd a) !! (fst a)) then 1 else 0) coords) 


generateCoordList :: Int -> Int -> [[Bool]] -> [(Int, Int)]
generateCoordList x y paperGrid = 
    let yLen = length paperGrid
        xLen = length $ paperGrid !! 0
        yCoords
            | (y == yLen-1) = [y-1..y]
            | (y == 0) = [0..y+1]
            | otherwise = [y-1..y+1]
        xCoords
            | (x == xLen-1) = [x-1..x]
            | (x == 0) = [0..x+1]
            | otherwise = [x-1..x+1]
    in filter (\coord -> not ((fst coord) == y && (snd coord) == x)) $ concat $ fmap (\x -> zip (repeat x) xCoords) yCoords 

-- For part 2 
countAmount :: [[Bool]] -> Int
countAmount paperGrid = 
    let coords = createCoords (length $ paperGrid !! 1) (length paperGrid)
        flatCoords = concat coords
        flatCoordsToCheck = filter (\x -> paperGrid !! (fst x) !! (snd x)) flatCoords
        currentCount = length $ filter (<4) $ fmap (\x -> getNeighboursCount (fst x) (snd x) paperGrid) flatCoordsToCheck
    in if (currentCount == 0) then 0 else currentCount + countAmount (getNextState paperGrid)


getNextState :: [[Bool]] -> [[Bool]]
getNextState paperGrid = 
    let coords = createCoords (length $ paperGrid !! 1) (length paperGrid)
        flatCoords = concat coords
        flatCoordsToCheck = filter (\x -> paperGrid !! (fst x) !! (snd x)) flatCoords
        coordsThatFail = filter (\x -> ((getNeighboursCount (fst x) (snd x) paperGrid) < 4)) flatCoordsToCheck
        yLen = length paperGrid
        xLen = length $ paperGrid !! 0
        -- just accept that we are going to have to slap this shit together ughh
        flatGrid = zip (flatCoords) $ (concat paperGrid)
        nextFlatGrid = fmap (\x -> 
            let coord = (fst x)
                val = (snd x)
                xCoord = (fst coord)
                yCoord = (snd coord)
            in if (not val) then False
                else (
                    if (elem coord coordsThatFail) then False
                    else True
                )
            ) flatGrid 
        in getGridBack yLen xLen nextFlatGrid

getGridBack :: Int -> Int -> [Bool] -> [[Bool]]
getGridBack 0 xLen flatGrid = []
getGridBack yLen xLen flatGrid = (take xLen flatGrid) : (getGridBack (yLen-1) xLen $ (drop xLen flatGrid))

