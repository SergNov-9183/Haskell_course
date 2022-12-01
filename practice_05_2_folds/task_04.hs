infixl 9 !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n 

fun :: a -> (Int -> Maybe a) -> Int -> Maybe a
fun x g n | n < 0  = Nothing
fun x g n | n == 0 = Just x
fun x g n | n > 0  = g (n - 1)
ini :: Int -> Maybe a
ini = const Nothing