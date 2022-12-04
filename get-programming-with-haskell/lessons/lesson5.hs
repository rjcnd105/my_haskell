
ifEven f x = if even x then f x else x

ifEvenInc x = ifEven (\x -> x + 1)


genIfXEven x = (\f -> ifEven f x)

main = do 
    putStrLn $ "ifEvenInc 5: " ++ show (genIfXEven 5 (\x -> x + 1))