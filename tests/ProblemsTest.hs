module Main where


import Problems
import Control.Exception (evaluate, ErrorCall(..))
import Test.HUnit
import qualified System.Exit as Exit

-- Problem 1
testMyLastSimpleList :: Test
testMyLastSimpleList = TestCase (assertEqual "should return 1" (Just 1) (myLast [5, 4, 3, 2, 1]))

testMyLastEmptyList :: Test
testMyLastEmptyList = TestCase $ do
    let result = myLast []
    assertEqual "should return Nothing" (Nothing :: Maybe Int) result

testMyLastSingleElemList :: Test
testMyLastSingleElemList = TestCase $ do
    let result = myLast [10]
    assertEqual "should return Just 10" (Just 10) result

-- Problem 2
testMyButLastSimpleList :: Test
testMyButLastSimpleList = TestCase (assertEqual "should return 2" (Just 2) (myButLast [5, 4, 3, 2, 1]))

testMyButLastEmptyList :: Test
testMyButLastEmptyList = TestCase $ do
    let result = myButLast []
    assertEqual "should return Nothing" (Nothing :: Maybe Int) result

testMyLastButSingleElemList :: Test
testMyLastButSingleElemList = TestCase $ do
    let result = myButLast [10]
    assertEqual "should return Nothing" (Nothing :: Maybe Int) result

-- Problem 3
testElementAtSimple :: Test
testElementAtSimple = TestCase $ do
    let result = elementAt [1, 2, 3, 4] 3
    assertEqual "should return 3" (Right 3) result

testElementAtIndexOutOfRange :: Test
testElementAtIndexOutOfRange = TestCase $ do
    let result = elementAt [1, 2] 3
    assertEqual "should return error" (Left "Index out of bounds") result

testElementAtZeroIndex :: Test
testElementAtZeroIndex = TestCase $ do
    let result = elementAt [1, 2] 0
    assertEqual "should return Nothing" (Left "No 0 indexing") result

-- Problem 4
testMyLengthString :: Test
testMyLengthString = TestCase $ do
    let result = myLength "Hello, world!"
    assertEqual "should return 13" 13 result

testMyLengthList :: Test
testMyLengthList = TestCase $ do
    let result = myLength [123, 456, 789]
    assertEqual "should return 3" 3 result

testMyLengthEmptyString :: Test
testMyLengthEmptyString = TestCase $ do
    let result = myLength ""
    assertEqual "should return 0" 0 result

testMyLengthEmptyList :: Test
testMyLengthEmptyList = TestCase $ do
    let result = myLength []
    assertEqual "should return 0" 0 result

-- Problem 5
testMyReverseList :: Test
testMyReverseList = TestCase $ do
    let result = myReverse [1,2,3,4]
    assertEqual "should return [4,3,2,1]" [4,3,2,1] result

testMyReverseString :: Test
testMyReverseString = TestCase $ do
    let result = myReverse "A man, a plan, a canal, panama!"
    assertEqual "should return '!amanap ,lanac a ,nalp a ,nam A'" "!amanap ,lanac a ,nalp a ,nam A" result

testMyReverseEmpty :: Test
testMyReverseEmpty = TestCase $ do
    let result = myReverse ""
    assertEqual "should return ''" "" result

-- Problem 6
testIsPalindromeSimpleList :: Test
testIsPalindromeSimpleList = TestCase $ do
    let result = isPalindrome [1,2,3]
    assertEqual "should return False" False result

testIsPalindromeList :: Test
testIsPalindromeList = TestCase $ do
    let result = isPalindrome [1,2,4,8,16,8,4,2,1]
    assertEqual "should return True" True result

testIsPalindromeString :: Test
testIsPalindromeString = TestCase $ do
    let result = isPalindrome "madamimadam"
    assertEqual "should return True" True result

-- Problem 7
testFlattenSingleElem :: Test
testFlattenSingleElem = TestCase $ do
    let result = flatten (Elem 5)
    assertEqual "should return [5]" [5] result

testFlattenEmptyList :: Test
testFlattenEmptyList = TestCase $ do
    let result = flatten (List [])
    assertEqual "should return []" ([] :: [Integer]) result

testFlattenNestedList :: Test
testFlattenNestedList = TestCase $ do
    let result = flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    assertEqual "should return []" [1,2,3,4,5] result

-- Problem 8
testCompressEmptyString :: Test
testCompressEmptyString = TestCase $ do
    let result = compress ""
    assertEqual "should return ''" "" result

testCompressEmptyList :: Test
testCompressEmptyList = TestCase $ do
    let result = compress [] :: [Integer]
    assertEqual "should return []" [] result

testCompressString :: Test
testCompressString = TestCase $ do
    let result = compress "aaaabccaadeeee"
    assertEqual "should return abcade" "abcade" result

testCompressList :: Test
testCompressList = TestCase $ do
    let result = compress [1, 1, 2, 1, 1, 3, 3, 2, 2]
    assertEqual "should return [1, 2, 1, 3, 2]" [1, 2, 1, 3, 2]  result


tests :: Test
tests = TestList [
    TestLabel "testMyLastSimpleList" testMyLastSimpleList,
    TestLabel "testMyLastEmptyList" testMyLastEmptyList,
    TestLabel "testMyLastSingleElemList" testMyLastSingleElemList,
    TestLabel "testMyButLastSimpleList" testMyButLastSimpleList,
    TestLabel "testMyButLastEmptyList" testMyButLastEmptyList,
    TestLabel "testMyLastButSingleElemList" testMyLastButSingleElemList,
    TestLabel "testElementAtSimple" testElementAtSimple,
    TestLabel "testElementAtIndexOutOfRange" testElementAtIndexOutOfRange,
    TestLabel "testElementAtZeroIndex" testElementAtZeroIndex,
    TestLabel "testMyLengthString" testMyLengthString,
    TestLabel "testMyLengthList" testMyLengthList,
    TestLabel "testMyLengthEmptyString" testMyLengthEmptyString,
    TestLabel "testMyLengthEmptyList" testMyLengthEmptyList,
    TestLabel "testMyReverseList" testMyReverseList,
    TestLabel "testMyReverseString" testMyReverseString,
    TestLabel "testMyReverseEmpty" testMyReverseEmpty,
    TestLabel "testIsPalindromeSimpleList" testIsPalindromeSimpleList,
    TestLabel "testIsPalindromeList" testIsPalindromeList,
    TestLabel "testIsPalindromeString" testIsPalindromeString,
    TestLabel "testFlattenSingleElem" testFlattenSingleElem,
    TestLabel "testFlattenEmptyList" testFlattenEmptyList,
    TestLabel "testFlattenNestedList" testFlattenNestedList
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
