{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}
-- language extension 확인
-- :show language

-- 문자를 반만큼 회전시키는 예제
-- 회전 시킨 문자를 다시 회전 시키면 원래의 값이 나와여 한다.

-- 회전시킬 문자들
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

-- 문자를 회전하는 함수
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation 
      where halfAlphabet = alphabetSize `div` 2 
            offset = fromEnum c + halfAlphabet
            rotation =  offset `mod` alphabetSize

-- ghci> rotN 4 L4
-- L2
-- ghci> rotN 4 L2
-- L4



-- Enum의 사이즈를 구할 때
-- 이러면 타입을 알 수 없어서 에러가 난다.
-- maxSize :: (Bounded a, Enum a) => a -> Int
-- maxSize a = fromEnum (maxBound a)
-- maxSize :: (Bounded a, Enum a) => a -> Int
-- maxSize = fromEnum  

-- 첫번째 방법. asTypeOf로 약간 꼼수처럼 작업할 수 있다.
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:asTypeOf
maxSize :: (Bounded a, Enum a) => a -> Int
maxSize a = 1 + fromEnum (asTypeOf maxBound a)

-- 두번째 방법.
-- 더 좋은 방법은 TypeApplications extension으로 작업하는 것이다.
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html#extension-TypeApplications
maxSize1 :: forall a . (Bounded a, Enum a) => Int
maxSize1 = 1 + fromEnum (maxBound @a)

message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L1,L2]

v = 10
-- ghci에서 실행되는데 왜 vscode 에디터에서 에러를 내뱉지?
rot4 = rotN 4 :: Int

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
      where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
            rot4l = rotN alphaSize

-- 다시 원상복구 시킬 수 있다. 그러나 이 경우는 알파벳이 짝수인 경우에만 가능하다.
-- ghci> fourLetterEncoder [L2,L3,L3,L3,L1,L1]
-- [L4,L1,L1,L1,L3,L3]
-- ghci> fourLetterEncoder [L4,L1,L1,L1,L3,L3]
-- [L2,L3,L3,L3,L1,L1]


data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show,Enum,Bounded)

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
      where halfN = n `div` 2
            offset = if even n
                  then fromEnum c + halfN
                  else 1 + fromEnum c + halfN
            rotation =  offset `mod` n

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha,Alpha,Beta,Alpha,Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals =  map rot3l vals
      where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
            rot3l = rotN alphaSize

