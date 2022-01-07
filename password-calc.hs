import Data.List

-- Helper Function to convert String -> [Int] , e.g. convertStrtoIntList "12345" -> [1,2,3,4,5]
convertStrtoIntList xs = map (read . (:"")) xs :: [Int]

checkForIncreasingNumbers [] = False
checkForIncreasingNumbers [x] = True
checkForIncreasingNumbers (x:y:xs)
  | y >= x = True && checkForIncreasingNumbers (y:xs)
  | otherwise = False && checkForIncreasingNumbers (y:xs)

checkForAdjacentNumbers xs = any (>1) . map (length) . group $ xs

checkValidNumber x
  | (checkForIncreasingNumbers xs) && (checkForAdjacentNumbers xs) = True
  | otherwise = False
  where xs = convertStrtoIntList $ show x

noOfPasswords lower upper = length [ x | x <- [lower..upper], checkValidNumber x ]

