-- Int
-- Int의 범위는 [-2^29 .. 2^29-1]
x :: Int 
x = 2
x2 = x * 2000 -- 4000
x3 = x ^ 2000 -- 0 , 범위를 초과하면 0을 반환

-- Integer
y :: Integer
y = 2
y2 = y * 2000 -- 4000
y3 = y ^ 2000 -- 0


half :: Int -> Double
half n = fromIntegral n / 2

-- Q
halve :: Int -> Int
halve = (`div` 2)

-- Q
-- Write a function printDouble that takes an Int and returns that value doubled as a string.
printDouble :: Int -> String
printDouble n = show (n * 2)
