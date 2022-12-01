tails' :: [a] -> [[a]]
tails' = foldr fun ini
fun = ( \ x y -> (x : (head y)) : y)
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'
fun' = ((([] :) .) . map . (:))
ini' = [[]]