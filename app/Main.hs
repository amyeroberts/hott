module Main where


-- Problem 1
myLast :: [a] -> Maybe a
myLast [x] = Just x
myLast (_:xs) = myLast xs
myLast [] = Nothing

-- Problem 2
myButLast :: [a] -> Maybe a
myButLast [x, _] = Just x
myButLast (_:xs) = myButLast xs
myButLast [] = Nothing

-- Problem 3
elementAt :: [a] -> Int -> Maybe a
elementAt (x:_) 0 = Just x
elementAt (_:xs) n = elementAt xs (n - 1)
elementAt [] _ = Nothing

-- Problem 4
myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength [] = 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]


main :: IO ()
main = do
    -- Problem
    let one = myLast [1, 2, 3]
    print one

    let two = myButLast [1, 2, 3]
    print two

    let three = elementAt [1, 2, 3, 4, 5] 3
    print three

    let four = myLength ["hi", "there", "friend"]
    print four

    let five = myReverse [1,2,3,4,5]
    print five
