{-
Name: Katie Miller 
Collaborators: none 
Notes: Haskell rocks 
-}

module HW01 where

isThisWorking :: String
isThisWorking = "Yes"

lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)
-- lastDigit = read . (:[]) . last . show

dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10) 

toDigits :: Integer -> [Integer]
toDigits = toDigits' []

-- Removes leading zeroes
toDigits' :: [Integer] -> Integer -> [Integer]
toDigits' acc i | i <= 0 = acc 
                | otherwise = toDigits' (lastDigit i : acc) (dropLastDigit i)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1, 2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = foldr (+) 0

validate :: Integer -> Bool
validate num = (checksum num) `mod` 10 == 0
               where checksum = sumDigits . doubleEveryOther . toDigits

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = if n <= 0 then [] else hanoi' n a b c

hanoi' :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi' 1 src dst _ = [(src, dst)]
hanoi' n src dst tmp = (hanoi' (n-1) src tmp dst) ++ [(src, dst)] ++ (hanoi' (n-1) tmp dst src)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d = if n <= 0 then [] else hanoi4' n a b c d

hanoi4' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4' 1 src dst _ _ = [(src, dst)]
hanoi4' 2 src dst tmp _ = [(src, tmp), (src, dst), (tmp, dst)] 
hanoi4' 3 src dst tmp1 tmp2 = [(src, tmp1), (src, tmp2), (src, dst), (tmp2, dst), (tmp1, dst)] 
hanoi4' n src dst tmp1 tmp2 = (hanoi4' (n-3) src tmp1 dst tmp2) ++ (hanoi' 3 src dst tmp2) ++ (hanoi4' (n-3) tmp1 dst src tmp2)

