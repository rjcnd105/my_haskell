module StudyBasic where



-- Enum
data XXX = AA|BB|CC|DD deriving (Enum, Show)



quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted
    


divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

applyTwice f x = f (f x)  

flip' :: (a -> b -> c) -> (b -> a -> c)  

-- 아래처럼 쓸 수 있다.
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map f xs  

largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0
    
