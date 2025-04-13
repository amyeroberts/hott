module Main where

import Problems
import Problems (NestedList(List))


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
    print (flatten (Elem 5))
    print (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
    
