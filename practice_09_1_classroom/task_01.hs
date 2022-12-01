data Logged a = Logged String a
    deriving (Eq, Show)

instance Functor Logged where
    fmap f (Logged s a) = Logged s (f a)

instance Applicative Logged where
    pure x = Logged mempty x
    (<*>) (Logged s f) (Logged s' a) = Logged s (f a)

instance Monad Logged where
    return = pure
    (>>=) (Logged s a) f = Logged (fst (unc (f a)) ++ s) (snd (unc (f a)))

unc :: Logged a -> (String, a)
unc (Logged s a) = (s, a)

write2log :: String -> Logged ()
write2log s = Logged s ()