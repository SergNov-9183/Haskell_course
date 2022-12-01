digits :: Integer -> [Integer]
digits n = map (\x -> read [x] :: Integer) (show (abs(n)))