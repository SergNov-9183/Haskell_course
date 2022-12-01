data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure x = Tr x x x
    (Tr x1 y1 z1) <*> (Tr x2 y2 z2) = Tr (x1 x2) (y1 y2) (z1 z2)

instance Foldable Triple where
    foldr f ini (Tr x y z) = f x (f y (f z ini))

instance Traversable Triple where
    traverse f (Tr x y z) = Tr <$> (f x) <*> (f y) <*> (f z)