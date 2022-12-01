import Control.Applicative

newtype Parser a = Parser { apply :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser p) = Parser (\s -> [(f a, s1) | (a, s1) <- p s])

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  u <*> v = Parser $ \s -> [(f a, s2) | (f, s1) <- apply u s, (a, s2) <- apply v s1]

instance Alternative Parser where
  empty = Parser (const [])
  u <|> v = Parser (\s -> apply u s ++ apply v s)