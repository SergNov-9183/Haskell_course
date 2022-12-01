absDiff :: Num a => [a] -> [a]
absDiff [x, y] = [abs $ x - y]
absDiff (x:y:xs) = do
    ys <- return $ absDiff $ y:xs
    z <- return $ abs $ x - y    
    z:ys
absDiff _ = []