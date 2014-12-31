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


