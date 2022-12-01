{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad
import Control.Monad.Except
import Data.Char

data ListIndexError =
    ErrTooLargeIndex Int
    | ErrNegativeIndex
    | OtherErr String
    deriving (Eq, Show)

atLeast :: Int -> [a] -> Bool
atLeast 0 _      = True
atLeast _ []     = False
atLeast n (_:ys) = atLeast (n-1) ys

infixl 9 !!!
(!!!) :: (MonadError ListIndexError m) => [a] -> Int -> m a
xs !!! n
    | n < 0 = throwError ErrNegativeIndex
    | atLeast (n+1) xs = return (xs !! n)
    | otherwise = throwError $ ErrTooLargeIndex n