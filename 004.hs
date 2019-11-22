{-# LANGUAGE TupleSections #-}

module ProjectEuler004
  ( palindrome
  , largestPalindrome
  , run
  ) where

import Data.List

palindrome :: Show a => a -> Bool
palindrome x = y == reverse y
  where
    y = show x

largestPalindrome :: Int -> Int
largestPalindrome m = uncurry (*) $ head filteredMatrix
  where
    digits = [m,m - 1 .. 1]
    matrix = digits >>= (\a -> map (a, ) digits)
    sortedMatrix = sortOn (\(a, b) -> -a - b) matrix
    filteredMatrix = filter (\(a, b) -> palindrome (a * b)) sortedMatrix

run :: IO ()
run = print $ largestPalindrome 999
