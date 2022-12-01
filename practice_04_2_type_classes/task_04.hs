rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = case drop n xs of 
    [] -> helper (mod n $ length xs) xs
    _  -> helper n xs
    
helper _ [] = []
helper 0 xs = xs
helper n xs | n > 0 = drop n xs ++ take n xs
            | n < 0 = let shift = length xs - (-n) in helper shift xs