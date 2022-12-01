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


--------------------------------------
askStrRdr :: Monad m => StrRdrT m String
askStrRdr = StrRdrT $ \str -> return str

asksStrRdr :: Monad m => (String -> a) -> StrRdrT m a
asksStrRdr f = do
  str <- askStrRdr
  return $ f str

type StrRdr = StrRdrT Identity

runStrRdr :: StrRdr a -> String -> a
runStrRdr (StrRdrT f) str = runIdentity idA
  where
    -- idA :: Identity a
    idA = f str

srtTst' :: StrRdr (String,Int)
srtTst'  = do
  env <- askStrRdr
  len <- asksStrRdr length
  return (env,len)

stSrTst :: StateT Int StrRdr Int
stSrTst = do
  a <- get
  n <- lift $ asksStrRdr length
  modify (+n)
  return a


--------------------------------------
instance MonadTrans StrRdrT where
  lift ma = StrRdrT $ \_ -> ma


--------------------------------------

instance MonadState s m => MonadState s (StrRdrT m) where
  get   = lift get
  put s = lift $ put s
  state f = lift $ state f