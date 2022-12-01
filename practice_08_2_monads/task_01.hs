surround :: a -> a -> [a] -> [a]
surround x y zs = do
  concatMap (\t -> [x, t, y]) zs