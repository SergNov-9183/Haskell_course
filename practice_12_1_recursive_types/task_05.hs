data T v x = Leaf | Branch x v x deriving (Eq,Show)
 
instance Functor (T a) where
    fmap f Leaf = Leaf
    fmap f (Branch b1 v b2) = Branch (f b1) v (f b2)
 
type Tree v = Fix (T v)
 
phiTInorder :: Algebra (T a) [a] -- T a [a] -> [a]
phiTInorder Leaf = []
phiTInorder (Branch b1 v b2) = b1 ++ [v] ++ b2
 
tree2listInorder :: Tree a -> [a] 
tree2listInorder = cata phiTInorder
 
psiTBST :: Ord a => Coalgebra (T a) [a]    -- [a] -> T a [a] 
psiTBST [] = Leaf
psiTBST (x:xs) = Branch b1 x b2
    where
      b1 = filter (<= x) xs
      b2 = filter (> x) xs
 
list2BST :: Ord a => [a] -> Tree a
list2BST = ana psiTBST
 
sort :: Ord a => [a] -> [a]
sort = hylo phiTInorder psiTBST