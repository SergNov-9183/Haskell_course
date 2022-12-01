import Data.Traversable (foldMapDefault)

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

testTree = Branch (Branch (Branch Nil (Just 1) Nil) (Just 2) (Branch Nil (Just 3) Nil)) (Just 4) (Branch Nil (Just 5) Nil)

instance Foldable Tree where
  foldMap = foldMapDefault

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Traversable Tree where    
    sequenceA Nil = pure Nil
    sequenceA (Branch l x r) = (\ left right root -> Branch left root right) <$> sequenceA l <*> sequenceA r <*> x