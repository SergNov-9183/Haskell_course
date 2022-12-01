import Data.Foldable
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show


instance Read a => Read (Tree a) where
  readsPrec _ = parseTree

parseTree :: Read a => String -> [(Tree a, String)]
parseTree ('{' : '}' : rest) = [(Leaf, rest)]
parseTree ('<' : rest)       =
  fold (fmap
    (\(left , rest') ->
      fold (fmap
        (\(x, rest'') ->
          fold
          (fmap
            (\(right, rest''') -> case rest''' of
              ('>': rest'''') -> [(Node left x right, rest'''')]
              _               -> []
            )
            (parseTree rest'')
          )
        )
        (readsPrec 0 rest'))
    )
    (parseTree rest))
parseTree _ = []