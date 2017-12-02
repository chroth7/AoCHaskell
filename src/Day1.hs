module Day1 ( day1e1, day1e2) where

import Data.Char (digitToInt)

-- todo use a better fail safe function
getDigits :: String -> [Int]
getDigits = map digitToInt

appendToEnd :: [a] -> [a]
appendToEnd [] = []
appendToEnd (x:xs) = (x:xs)++[x]

reduceToSum :: [Int] -> Int
reduceToSum (x:y:xs) = useX + reduceToSum (y:xs)
  where
    useX =
      if x == y
        then x
        else 0
reduceToSum [x] = 0
reduceToSum [] = 0

day1e1 :: String -> Int
day1e1 = reduceToSum . appendToEnd . getDigits

--- PART TWO
-- brute force
increment2 :: [Int] -> Int -> Int
increment2 list@(x:xs) i =
  if list !! i == list !! mod (i + div (length list)  2) (length list)
    then list !! i
    else 0


foldIt :: [Int] -> Int
foldIt list =
  fst $ foldl (\a v -> (fst a + increment2 list (snd a), snd a + 1))  (0,0) list

day1e2 :: String -> Int
day1e2 = foldIt . getDigits
