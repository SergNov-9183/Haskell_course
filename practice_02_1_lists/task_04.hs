digits :: Integer -> [Integer]
digits x
  | x > 0  = go x
  | x < 0  = go (-x)
  | x == 0 = 0 : []
  where
    go :: Integer -> [Integer]
    go n
      | n > 0  = go (n `div` 10) ++ n `mod` 10 : []
      | n == 0 = []

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes x
  | length (digits x) > 8 = length (intersect [1..9] (digits x)) == 9
  | otherwise = False
  where
    intersect :: [Integer] -> [Integer] -> [Integer]
    intersect xs ys = filter (\x -> x `elem` xs) ys