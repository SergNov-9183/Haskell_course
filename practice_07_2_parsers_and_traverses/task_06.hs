class Functor f => Monoidal f where
  unit  :: f ()
  (*&*) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
  unit = Just ()
  Nothing *&* _ = Nothing
  _ *&* Nothing = Nothing
  (Just a) *&* (Just b) = Just (a,b)

instance Monoid s => Monoidal ((,) s) where
  unit = (mempty, ())
  (f,a) *&* (g,b) = (mappend f g, (a,b))

instance Monoidal ((->) e) where
  unit = pure mempty
  f *&* g = pure (,) <*> f <*> g