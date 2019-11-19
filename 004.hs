module ProjectEuler004
  (
  ) where

palindrome :: Show a => a -> Bool
palindrome x = y == reverse y
  where
    y = show x

twoDigits = [99,98 .. 1]

largesest m = head [x | x <- [m,m - 1 .. 2], palindrome x]

main004 = print $ largesest 1000
