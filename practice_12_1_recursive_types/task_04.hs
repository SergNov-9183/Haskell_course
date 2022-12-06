data T v x = Leaf | Branch x v x deriving (Eq,Show)
 
instance Functor (T a) where
    fmap f Leaf = Leaf
    fmap f (Branch b1 v b2) = Branch (f b1) v (f b2)
 
type Tree v = Fix (T v)
 
phiTSum :: Algebra (T Integer) Integer
phiTSum Leaf = 0
phiTSum (Branch b1 v b2) = b1 + v + b2
 
treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum