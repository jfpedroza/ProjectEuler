module ProjectEuler001
  ( run
  ) where

run :: IO ()
run = print $ sum [x | x <- [1 .. 999], x `mod` 3 == 0 || x `mod` 5 == 0]
