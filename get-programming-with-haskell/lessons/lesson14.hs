-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6



-- 패턴 매칭을 이용하여 이렇게도 선언 가능.
-- 다만 이러면 기존의 show를 덮어쓰므로 Show의 인스턴스로 선언하는 것.
-- show :: SixSidedDie -> String
-- show S1 = "one"
-- show S2 = "two"
-- show S3 = "three"
-- show S4 = "four"
-- show S5 = "five"
-- show S6 = "six"






data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq,Ord,Enum)

-- instance: implement type class
instance Show SixSidedDie where
   show S1 = "one"
   show S2 = "two"
   show S3 = "three"
   show S4 = "four"
   show S5 = "five"
   show S6 = "six"

-- ghci> S3
-- "three"

-- ghci> S1 < S2
-- true

-- ghci> [S1 .. S6]
-- [one,two,three,four,five,six]





data TwoSidedDie = SS1 | SS2

-- Error: Multiple declarations of 'show'
-- 이렇게 show를 정의하는 경우 다형성이 필요하다.
-- 이러면 기존의 Show가 덮어씌워져 버림.
-- show :: TwoSidedDie -> String
-- show One = "one"
-- show Two = "two"

newtype Name = Name (String, String) deriving (Eq)

instance Ord Name where
   compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

instance Show Name where
   show (Name (f,l)) = l ++ " " ++ f


person1 :: Name
person2 :: Name
person3 :: Name

person1 = Name ("Emil","Cioran")
person2 = Name ("Eugene","Thacker")
person3 = Name ("Friedrich","Nietzsche")
people :: [Name]
people = [person1, person2, person3]

-- ghci> person1
-- Cioran Emil

-- ghci> person2
-- Thacker Eugene

-- ghci> person2 > person3
-- True

-- ghci> people
-- [Cioran Emil,Thacker Eugene,Nietzsche Friedrich]

-- ghci> import Data.List
-- ghci> sort names
-- ["Cioran Emil", "Nietzsche Friedrich", "Thacker Eugene"]



-- Q14.1
data MyNumber = One | Two | Three deriving Enum

instance Eq MyNumber where
   (==) num1 num2 = (fromEnum num1) == (fromEnum num2)

instance Ord MyNumber where 
   (<=) num1 num2 = (fromEnum num1) <= (fromEnum num2)




-- Q14.2
data FiveDice = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Eq, Ord, Enum, Show)

class (Eq a, Enum a) => Dice a where
   roll :: Int -> a

instance Dice FiveDice where
   roll n = toEnum (n `mod` 5)

-- ghci> roll 5 :: FiveDice


