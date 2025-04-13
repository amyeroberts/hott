module Main where

import Problems


main :: IO ()
main = do
    -- Problems
    print (myLast [1, 2, 3])
    print (myButLast ([1, 2, 3, 4, 5] :: [Int]))
    print (elementAt [1, 2, 3, 4, 5] 3)
    print (myLength [1, 2, 3, 4, 5])
    print (myLength "Hello, world!")
    print (myReverse "Hello, world!")
    print (isPalindrome "hannah")
