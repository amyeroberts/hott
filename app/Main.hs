module Main where


-- Problem 1
myLast :: [a] -> Maybe a
myLast [x] = Just x
myLast (_:xs) = myLast xs
myLast [] = Nothing

main :: IO ()
main = do
    -- Problem
    let x = myLast [1, 2, 3]
    print x
    let y = myLast ([] :: [Int])
    print y
