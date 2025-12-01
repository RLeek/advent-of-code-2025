import Data.Maybe

-- So get list of input
-- convert to actions
-- for each action perform operation

-- keep track of each operation that equals 0 and return count

main :: IO Integer
main = do
    -- we'll worry about how this works later lmao :/
    -- contents <- readFile "day1.txt"
    -- convertToOperations (lines contents)
    return 1

data Operation = L Integer | R Integer 
    deriving (Show)


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