module ProjectEuler003
  ( sqrt'
  , isPrime
  , lastFactor
  , run
  ) where

sqrt' :: (Integral a) => a -> a
sqrt' n = floor $ sqrt $ fromIntegral n

isPrime :: (Integral a) => a -> Bool
isPrime 1 = False
isPrime n = null [() | x <- [2 .. sqrt' n], mod n x == 0]

lastFactor :: Integral a => a -> a
lastFactor n =
  let lastChecked = sqrt' n
      factors =
        [ y
        | x <- [lastChecked,lastChecked - 1 .. 2]
        , mod n x == 0
        , y <- [x, n `div` x]
        , isPrime y
        ]
   in maximum factors

run :: IO ()
run = print $ lastFactor 600851475143
