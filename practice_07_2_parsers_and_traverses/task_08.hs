pure' :: Monoidal f => a -> f a
pure' a = fmap (const a) unit

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' a b = fmap (uncurry ($)) $ a *&* b