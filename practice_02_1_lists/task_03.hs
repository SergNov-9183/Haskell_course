digits :: Integer -> [Integer]
digits n = map (\x -> read [x] :: Integer) (show (abs(n)))
containsAllDigits :: Integer -> Bool
containsAllDigits n = foldr (\ p acc -> acc && (p `elem` w)) True [1..9]
         where w = digits n