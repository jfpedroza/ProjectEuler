{-# LANGUAGE TupleSections #-}

module ProjectEuler004
  ( palindrome
  , largestPalindrome
  , largestPalindrome'
  , run
  ) where

import Data.List

palindrome :: Show a => a -> Bool
palindrome x = y == reverse y
  where
    y = show x

largestPalindrome :: [Int] -> Int
largestPalindrome range = uncurry (*) $ head filteredMatrix
  where
    m = last range
    matrix = range >>= (\a -> map (a, ) [a .. m])
    sortedMatrix = sortOn (\(a, b) -> -a - b) matrix
    filteredMatrix = filter (\(a, b) -> palindrome (a * b)) sortedMatrix

largestPalindrome' :: [Int] -> Int
largestPalindrome' range =
  maximum $ filter palindrome [a * b | a <- range, b <- [a .. last range]]

run :: IO ()
run = print $ largestPalindrome [100 .. 999]
