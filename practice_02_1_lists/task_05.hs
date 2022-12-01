sublist :: Int -> Int -> [a] -> [a]
sublist n m l
  | m > n     = drop n (take m l)
  | otherwise = take 0 l