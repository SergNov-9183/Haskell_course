movingLists :: Int -> [a] -> [[a]]
movingLists n l
  | length (take n l) == n = ((take n l) : []) ++ ((movingLists n (tail l)))
  | otherwise              = []