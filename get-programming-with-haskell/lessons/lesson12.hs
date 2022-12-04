type FirstName = String
type LastName = String
type Age = Int
type Height = Int

type PatientName = (FirstName, LastName)

firstName :: PatientName -> FirstName
lastName :: PatientName -> LastName

firstName = fst
lastName = snd

-- 남성이거나 여성의 인스턴스일 수 있습니다.
data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg
patient3BT :: BloodType
patient3BT = BloodType AB Pos


-- 현재로썬 출력할 수 있는 방법이 없으므로 추가해준다. (다음 챕터에서 더 나은 방법을 배움)
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh


-- Rh는 계산하지 않음
canDonateTo :: BloodType -> BloodType -> Bool
-- O형은 모든 형에 기부 가능
canDonateTo (BloodType O _) _ = True
-- 모든 형은 AB형에 기부 가능
canDonateTo _ (BloodType AB _) = True
-- A형은 A, AB형에 기부 가능
canDonateTo (BloodType A _) (BloodType A _) = True
-- B형은 B, AB형에 기부 가능
canDonateTo (BloodType B _) (BloodType B _) = True
-- 위의 패턴이 아닌 나머지는 기부 불가능
canDonateTo _ _ = False --otherwise


type MiddleName = String
data Name = Name FirstName LastName 
  | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

jackieSmith :: Patient
jackieSmith = Patient {name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }

{-
  **************
  Patient Name: Smith, John
  Sex: Male
  Age: 46
  Height: 72 in.
  Weight: 210 lbs.
  Blood Type: AB+
-}


-- Q12.1
-- patientCanDonateTo :: Patient -> Patient -> Bool
-- patientCanDonateTo (bloodType Patient) to = 

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

-- Q12.2
patientSummary :: Patient -> String
patientSummary patient =  "**************\n" ++ 
  "Patient Name: " ++ showName (name patient) ++ "\n" ++
  "Sex: " ++ showSex (sex patient) ++ "\n" ++
  "Age: " ++ show (age patient) ++ "\n" ++
  "Height: " ++ show (height patient) ++ " in.\n" ++
  "Weight: " ++ show (weight patient) ++ " lbs.\n" ++
  "Blood Type: " ++ showBloodType (bloodType patient) 