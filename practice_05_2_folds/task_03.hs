import Data.List

reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
fun' = (\x acc -> acc ++ [x])
ini' = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''
fun'' = (\a x -> x:a)
ini'' = []