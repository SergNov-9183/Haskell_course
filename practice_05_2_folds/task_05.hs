import Data.List

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f a bs = foldr (fun f) ini bs a
fun f element g value = g (f value element)
ini = id 