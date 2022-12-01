doubleFact :: Integer -> Integer
doubleFact n = if n <= 0 then 1 else doubleFact (n - 2) * n