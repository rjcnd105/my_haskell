myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n - 1) xs


main = do
    putStrLn $ "myTake 3 [1,2,3,4,5]: " ++ show (myTake 3 [1,2,3,4,5]) 