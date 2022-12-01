sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] []     = []
sum3 xs [] []     = xs
sum3 [] ys []     = ys
sum3 [] [] zs     = zs

sum3 (x : xs) (y : ys) (z : zs) = x + y + z : rest
  where 
    rest = sum3 xs ys zs

sum3 [] (y : ys) (z : zs) = y + z : rest
  where 
    rest = sum3 [] ys zs

sum3 (x : xs) [] (z : zs) = x + z : rest
  where 
    rest = sum3 xs [] zs

sum3 (x : xs) (y : ys) [] = x + y : rest
  where 
    rest = sum3 xs ys []