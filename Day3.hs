-- find the largest value


-- given a string, convert each char into an integer,


-- find the largest int (excluding last element) -- with index
-- Then find larget remaining number -- and that is the largest number


main:: IO Int
main = do
    contents <- readFile "Day3.txt"
    let numLists = fmap (convertToNumList) $ lines contents
        firstMatch = fmap (findMaxNumIndex.init) numLists
        firstIndex = fmap (fst) firstMatch
        dropFirstIndex = \x -> drop (fst x+1) (snd x) -- drops the first set of elements
        secondMatch = fmap findMaxNumIndex $ fmap dropFirstIndex $ zip firstIndex numLists
    return $ foldr (+) 0 $ (fmap (\x -> (snd $ (fst x) ) * 10 + snd (snd x))  $ zip firstMatch secondMatch)

-- Gives us a list of nums
convertToNumList:: String -> [Int]
convertToNumList text = map (\x -> read [x]::Int) text

findMaxNumIndex:: [Int] -> (Int, Int)
findMaxNumIndex nums = foldr replaceMaxNumIndex (0,0) $ zip [0..] nums


replaceMaxNumIndex:: (Int, Int) -> (Int, Int) ->  (Int, Int)
replaceMaxNumIndex (index, currNum) (x, y)  = 
    if (y > currNum) then (x,y) 
    else (index, currNum)



main2:: IO Int
main2 = do
    contents <- readFile "Day3.txt"
    let numLists = fmap (convertToNumList) $ lines contents
    return $ foldr (+) 0 $ fmap (findLargestNum 12) numLists 


findLargestNum:: Int -> [Int] -> Int
-- should go in loop, until Int is zero, each time finding the first most in list (excluding the last remaining count - until we return a list of matches
-- once we return a list of matches the nwe sum them
findLargestNum 0 _ = 0
findLargestNum currentIdx remainingList =
    let currentLargestNumIndex =  (\x -> findMaxNumIndex $ (take (length x - currentIdx +1)) x) remainingList
        currentLargestNum = snd currentLargestNumIndex
        currentLargestIndex = fst currentLargestNumIndex
    in (currentLargestNum * (10 ^ (currentIdx-1))) + (findLargestNum (currentIdx-1) $ drop (currentLargestIndex + 1) remainingList)


-- findMaxNum:: [Int] -> NumPos
-- findMaxNumIndex x = foldl  
                            


-- each iteration you take 12 and then match the rest
-- repeat this process to get your numbers and then sum them
-- wtaf