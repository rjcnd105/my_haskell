


body = (\sumSquare squareSum ->
     if sumSquare > squareSum
        then sumSquare
        else squareSum)


body2 sumSquare squareSum = if sumSquare > squareSum
        then sumSquare
        else squareSum

sumSquareOrSquareSum x y = body (x^2 + y^2) ((x+y)^2) 

doubleDouble x = (\dubs -> dubs*2) (x*2)




main = do 
    putStrLn $ "sumSquareOrSquareSum: " ++ show (sumSquareOrSquareSum 5 8)
    putStrLn $ "doubleDouble: " ++ show (doubleDouble 5)