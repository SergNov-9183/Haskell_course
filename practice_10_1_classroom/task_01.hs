{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
import           Control.Monad
import           Control.Monad.Identity (Identity (..))
import           Control.Monad.State

newtype StrRdrT m a = StrRdrT { runStrRdrT :: String -> m a }

instance Monad m => Monad (StrRdrT m) where
  return :: a -> StrRdrT m a
  return a = StrRdrT $ \str -> return a

  (>>=) :: StrRdrT m a -> (a -> StrRdrT m b) -> StrRdrT m b
  (>>=) (StrRdrT f) g = StrRdrT $ \str ->
      let ma {- :: m a -} = f str
          mb {- :: m b -} = ma >>= \a -> runStrRdrT (g a) str
      in mb

instance MonadFail m => MonadFail (StrRdrT m)  where
  fail :: String -> StrRdrT m a
  fail err = StrRdrT $ \str -> fail err

instance Monad m => Functor (StrRdrT m) where
  fmap f (StrRdrT g) = StrRdrT $ \str -> fmap f (g str)
instance Monad m => Applicative (StrRdrT m) where
  pure = return
  (<*>) = ap

srtTst :: StrRdrT Identity Int
srtTst = do
  x <- StrRdrT $ Identity <$> length
  y <- return 10
  return $ x + y

failTst :: StrRdrT [] Integer
failTst = do
  'z' <- StrRdrT id
  return 42