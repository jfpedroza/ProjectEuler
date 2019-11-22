module ProjectEuler006
  ( squares
  , run
  ) where

squares :: Int -> Int
squares m = squareOfSum - sumOfSquares
  where
    square x = x * x
    squareOfSum = square $ sum [1 .. m]
    sumOfSquares = sum $ map square [1 .. m]

run :: IO ()
run = print $ squares 100
