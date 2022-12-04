
-- foldl를 이용한 reverse 구현
reverseCons x y = y:x
myReverse list = foldl reverseCons [] list




-- foldl 구현
myFoldl f init [] = init
myFoldl f init (x:[]) = f init x
myFoldl f init (x:xs) = myFoldl f (f init x) xs

myElem list el = if (length $ filter (\x -> x == el) list) > 0 then True else False



isMyPanama list = filter (\x -> x != ' ') list
    where noSpaceList = filter (\x -> x != ' ') list
          lowerCaseList = 

main = do putStrLn $ show (myReverse [1,2,3])