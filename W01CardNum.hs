module W01CardNum where

-- | Double the value of every second digit beginning from the right.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : xs)
  | (odd . length) xs = (2 * x) : doubleEveryOther xs
  | (even . length) xs = x : doubleEveryOther xs

-- | Sum all the digits in the number.
sumDigits :: [Integer] -> Integer
sumDigits arr = foldr (\x acc -> read [x] + acc) 0 (concatMap show arr)

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = error "Number must be positive"
  | otherwise = fmap (\x -> read [x]) (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

validate :: Integer -> Bool
validate x = rem (sumDigits $ doubleEveryOther $ toDigits x) 10 == 0