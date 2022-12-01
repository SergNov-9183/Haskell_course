data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show

treeSum :: Tree Integer -> Integer
treeSum Leaf     = 0
treeSum (Node left a right)  = a + treeSum left + treeSum right

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node left a rigth) = 1 + max (treeHeight left) (treeHeight rigth)