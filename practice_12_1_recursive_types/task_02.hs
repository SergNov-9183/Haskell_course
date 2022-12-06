data E x = Num Int | Add x x | Mult x x deriving (Eq,Show)
 
type Expr = Fix E
 
instance Functor E where
    fmap f (Num x) = Num x
    fmap f (Add x1 x2) = Add (f x1) (f x2)
    fmap f (Mult x1 x2) = Mult (f x1) (f x2)
 
phiE :: E Int -> Int
phiE (Num x) = x
phiE (Add x1 x2) = x1 + x2
phiE (Mult x1 x2) = x1 * x2
 
eval :: Expr -> Int
eval = cata phiE
 
phiEShow :: E String -> String
phiEShow (Num x) = show x
phiEShow (Add x1 x2) = "(" ++ x1 ++ "+" ++ x2 ++ ")"
phiEShow (Mult x1 x2) = "(" ++ x1 ++ "*" ++ x2 ++ ")"