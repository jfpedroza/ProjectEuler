fibonacci t1 t2 = t1:fibonacci t2 (t1+t2)

main = do
    print $ sum $ filter even $ takeWhile (<4000000) $ fibonacci 1 2
