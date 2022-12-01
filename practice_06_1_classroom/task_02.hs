{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

newtype Cmps f g x = Cmps { getCmps :: f (g x) }

instance (Functor f, Functor g) => Functor (Cmps f g) 
  where fmap f (Cmps u)= Cmps (fmap (fmap f) u)