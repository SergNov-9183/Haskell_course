sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x   | x == 0 = (0, 1)
                | otherwise = (a x, b x)
                    where 
                        b x | abs (abs x) < 10 = 1 
                            | otherwise = abs(1 + b (div (abs x) 10))
                        a x | abs (abs x) < 10 = abs(x)
                            | otherwise = abs(a (div (abs x) 10) + mod (abs x) 10)