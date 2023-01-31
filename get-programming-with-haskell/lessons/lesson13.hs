class DescribableKo a where
   describe :: a -> String

-- Ord를 파생시키면 데이터 생성자의 순서를 기준으로 매김. 즉 Vanilla가 Chocolate보다 더 크다 
data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- 아래와 같이 Num 타입클래스를 받도록 만들면 Int, Double 뿐 아니라
addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2


cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound 
              else succ n