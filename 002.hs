module ProjectEuler002
  ( run
  , fibonacci
  ) where

fibonacci :: Num a => a -> a -> [a]
fibonacci t1 t2 = t1 : fibonacci t2 (t1 + t2)

run :: IO ()
run = print $ sum $ filter even $ takeWhile (< 4000000) $ fibonacci 1 1
