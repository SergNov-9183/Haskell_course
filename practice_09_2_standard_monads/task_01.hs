import Control.Monad.Writer
 
minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR ini [] = writer (ini, show ini)
minusLoggedR ini (x:xs) = do
      tell $ "(" ++ (show x) ++ "-" ++ str ++ ")"
      return $ x - res
      where (res, str) = runWriter $ minusLoggedR ini xs