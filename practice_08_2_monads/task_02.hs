lookups :: (Eq k) => k -> [(k,v)] -> [v]
lookups x ys = do
 concatMap (\t -> [snd t | fst t == x]) ys