module ProjectEuler005
  ( evenlyDivisible
  , run
  ) where

evenlyDivisible :: Int -> Int
evenlyDivisible limit = head [x | x <- [limit,limit + limit ..], evenly x]
  where
    evenly x = all (\d -> x `rem` d == 0) [2 .. limit]

run :: IO ()
run = print $ evenlyDivisible 20
