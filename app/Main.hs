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
elementAt (x:_) 1 = Just x
elementAt (_:xs) z = elementAt xs  (z - 1)
elementAt [] _ = Nothing

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:y) = 1 + myLength y

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

main :: IO ()
main = do
    -- -- Problem
    -- let x = myLast [1, 2, 3]
    -- print x
    -- let y = myLast ([] :: [Int])
    -- print y

    -- let z = myButLast ([1, 2, 3, 4, 5] :: [Int])
    -- print z

    -- let x = elementAt [1, 2, 3, 4, 5] 3
    -- print x

    -- let x = myLength [1, 2, 3, 4, 5]
    -- let x = myLength "Hello, world!"
    -- print x

    let x = myReverse "Hello, world!"
    print x
