
main :: IO Int
main = do 
    contents <- readFile "day2.txt"
    return $ sum $ fmap (sum.getInvalidNumbers) $ fmap convertToRange $ fmap (breakByTerm (== '-')) (breakByTerm (==',') contents)

breakByTerm :: (Char -> Bool) -> String -> [String]
breakByTerm predicate "" =  []
breakByTerm predicate text = let (firstMatch, rest) = break predicate text in
    (firstMatch : (breakByTerm predicate (drop 1 rest)))

convertToRange :: [String] -> Range
convertToRange (x:xs) = 
    let number1 = read x::Int
        number2 = read (head xs)::Int
    in Range number1 number2

getInvalidNumbers :: Range -> [Int]
getInvalidNumbers (Range x y) = 
    let numRange = [x..y] 
    --in filter getIsInvalid numRange
    in filter getIsInvalid2 numRange

getIsInvalid :: Int -> Bool
getIsInvalid num = 
    let stringNum = show num
        firstHalf = take (length stringNum `div` 2) stringNum
        secondHalf = drop (length stringNum `div` 2) stringNum
    in firstHalf == secondHalf 



-- For part 2

getIsInvalid2 :: Int -> Bool
getIsInvalid2 num = 
    let stringNum = show num
        divisibleList = getDivisibles $ fromIntegral $ length stringNum
    in  foldr (||) False $ fmap (\x -> getIsInvalidAtSplit x stringNum) divisibleList

getIsInvalidAtSplit :: Int -> String -> Bool
getIsInvalidAtSplit num string = 
    let stringNum = show num
        splitElements = splitAtX num string
    in allEqual splitElements

splitAtX :: Int -> String -> [String]
splitAtX num [] = []
splitAtX num x = (take num x):(splitAtX num (drop num x))


allEqual :: [String] -> Bool
allEqual (x:[]) = True
allEqual (x:xs) = x == (head xs) && allEqual xs


getDivisibles :: Int -> [Int]
getDivisibles x = 
    let validOnes = [1..x-1] in
    filter (\y -> rem x y == 0) validOnes

data Range = Range Int Int deriving (Show)


