{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
 
data B v = Empty | Zero v | One v deriving (Eq,Show)
 
type Bin = Fix B
  
bin2int :: Bin -> Int
bin2int = cata phiB
 
int2bin :: Int -> Bin
int2bin = ana psiB
 
instance Functor B where
    fmap _ Empty = Empty
    fmap f (Zero b) = Zero (f b)
    fmap f (One b) = One (f b)
 
phiB :: B Int -> Int
phiB Empty = 0
phiB (Zero x) = 2 * x
phiB (One x) = 2 * x + 1
 
psiB :: Int -> B Int
psiB 0 = Empty
psiB x = if (mod x 2 == 0) then Zero (div x 2) else One (div x 2)