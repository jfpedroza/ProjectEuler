sqrt' :: (Integral a) => a -> a
sqrt' n = floor $ sqrt $ fromIntegral n

isPrime :: (Integral a) => a -> Bool
isPrime 1 = False
isPrime n = null [() | x <- [2..sqrt' n], mod n x == 0]

lastFactor n = 
    let last = sqrt' n
        factors = [x | x <- [last,last-1..2], mod n x == 0 && isPrime x]
     in head factors

main = print $ lastFactor 600851475143
