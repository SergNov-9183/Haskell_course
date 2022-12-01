data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)


instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
    pure x = Branch (pure x) x (pure x)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Branch fl f fr) <*> (Branch l x r) = Branch (fl <*> l) (f x) (fr <*> r)