module StudyFunction where
  
plus :: Int -> Int -> Int
plus a b = a + b


double x = x + x
quadruple x = double (double x)

factorial :: Int -> Int
factorial n = product [1..n]

-- Tuple
add :: (Int, Int) -> Int
add (x, y) = x + y

addC :: Int -> Int -> Int
addC a b = a + b

doubleSmallNumberPlus1 x = 
 (if    x > 100  
  then  x  
  else  x*2) + 1   

-- 일반적으로 이름에 '를 쓴다는 것은 lazy하지 않은 것을 뜻함.  
conanO'Brien = "It's a-me, Conan O'Brien!"   
                        
--
ifPlus :: Int -> Int -> Int
ifPlus a b
  | a < 0     = error "Incorrect input"
  | b < 10    = error "Incorrect input"
