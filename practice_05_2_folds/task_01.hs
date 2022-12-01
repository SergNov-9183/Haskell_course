import Data.List

revRange :: (Char,Char) -> [Char]

revRange = unfoldr fun 

fun = (\(a,b) -> if b < a then Nothing else Just (b,(a, pred b)))