module Problems where

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
elementAt :: [a] -> Int -> Either String a
elementAt _ 0 = Left "No 0 indexing"
elementAt (x:_) 1 = Right x
elementAt (_:xs) z = elementAt xs  (z - 1)
elementAt [] x = Left "Index out of bounds"

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:y) = 1 + myLength y

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == myReverse x

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:z)
    | x == y = compress (x:z)
    | otherwise = x:compress (y:z)
