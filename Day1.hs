import Data.Maybe

main :: IO Integer
main = do
    contents <- readFile "day1.txt"
    -- return (turnDial1 (convertToOperations (lines contents)) 50)
    return (turnDial2 (convertToOperations (lines contents)) 50)


data Operation = L Integer | R Integer 
    deriving (Show)

-- turnDial is used for part 1
turnDial :: [Operation] -> Integer -> Integer
turnDial [] curr = 0
turnDial (x:xs) curr = 
    let newcurr = case x of
            L y -> mod (curr - y) 100
            R y -> mod (curr + y) 100
    in case newcurr of
        0 -> (turnDial xs 0) + 1
        _ -> (turnDial xs newcurr)

-- turnDial is used for part 2
turnDial2 :: [Operation] -> Integer -> Integer
turnDial2 [] curr = 0
turnDial2 (x:xs) curr = 
    let newcurr = case x of
            L y -> mod (curr - y) 100
            R y -> mod (curr + y) 100
        rotations = case x of
            L y -> quot y 100 
            R y -> quot y 100
        turnedOver = case x of
            L y |  (newcurr - curr) > 0 && ((newcurr /= 0) && (curr /= 0)) -> 1
                |  otherwise -> 0
            R y | (newcurr - curr) < 0 && ((newcurr /= 0) && (curr /= 0)) -> 1
                | otherwise -> 0
        adjustBack = case x of
            L y | ((rem y 100) == 0 && newcurr == 0) -> 1
                | otherwise -> 0 
            R y | ((rem y 100) == 0 && newcurr == 0) -> 1
                | otherwise -> 0
    in case newcurr of
        0 -> (turnDial2 xs 0) + 1 + rotations + turnedOver - adjustBack
        _ -> (turnDial2 xs newcurr) + rotations + turnedOver

-- uses convertToOperation to convert from list of strings to list of operations
convertToOperations :: [String] -> [Operation]
convertToOperations lines = mapMaybe convertToOperation lines 

-- takes string -- validates it and then returns the corresponding operation
convertToOperation :: String -> Maybe Operation
convertToOperation (x:xs) =
    let number = read xs::Integer in
    case x of 
        'L'  -> Just (L number)
        'R'  -> Just (R number)
        _     -> Nothing
convertToOperation [] = Nothing
