integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b
  | a < b     = go f a b 0 ((b - a) / 1000)
  | a > b     = -1 * integration f b a
  | otherwise = 0
  where
    go f a b res step
      | (b - a) < step = res + walk f a b (b - a)
      | otherwise      = go f (a + step) b (res + walk f a (a + step) step) step
      where 
        walk f m n d =
          ((f m + f n) / 2) * d