import Data.List

names = [("Ian", "Curtis"),
         ("Bernard","Sumner"),
         ("Peter", "Hook"),
         ("Stephen","Morris")]

myCompareName name1 name2 = 
        if firstName1 > firstName2 then GT
        else if firstName1 < firstName2 then LT
        else if lastName1 > lastName2 then GT
        else if lastName1 < lastName2 then LT
        else EQ 
        where firstName1 = fst name1
              firstName2 = fst name2
              lastName1 = snd name1
              lastName2 = snd name2

mySortedNames = sortBy myCompareName names




sfOffice name = if lastName < "L"
                then nameText
                     ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name



getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location

addressLetter2 name location = getLocationFunction location name



main = do 
    putStrLn $ "mySortedNames: " ++ show mySortedNames
    -- mySortedNames: [("Bernard","Sumner"),("Ian","Curtis"),("Peter","Hook"),("Stephen","Morris")]
    putStrLn $ "addressLetter1: " ++ addressLetter ("Bob","Smith") "ny"
    putStrLn $ "addressLetter2: " ++ addressLetter2 ("Bob","Jones") "ny"
    putStrLn $ "addressLetter3: " ++ addressLetter2 ("Samantha","Smith") "sf"