repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem n lst = concatMap (\ x -> replicate n x) lst