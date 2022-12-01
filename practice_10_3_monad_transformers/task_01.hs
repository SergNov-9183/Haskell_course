{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Reader

data Logged a = Logged String a
    deriving (Eq, Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

type Logg = LoggT Identity

instance Monad m => Functor (LoggT m) where
    fmap = liftM

instance Monad m => Applicative (LoggT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (LoggT m) where
    return a = LoggT (return (Logged mempty a))
    (>>=) (LoggT a) b = LoggT $ do
        (Logged d a) <- a
        (Logged c b) <- runLoggT (b a)
        return (Logged (c `mappend` d) b)

instance MonadFail m => MonadFail (LoggT m) where
    fail = LoggT . fail