data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show

elemTree :: Eq a => a -> Tree a -> Bool
elemTree a aa = go [aa]
  where 
    --go :: [Tree a] -> Bool
    go [] = False
    go (tree :trees) = found || go (trees ++ others)
      where
        (found, others) = lookAtOneTree tree

    --lookAtOneTree :: Tree a -> (Bool, [Tree a])
    lookAtOneTree Leaf = (False, [])
    lookAtOneTree (Node left x right) = (x == a, [left, right])