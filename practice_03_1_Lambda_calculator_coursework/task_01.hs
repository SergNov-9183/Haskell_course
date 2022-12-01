import Data.List

type Symb = String 
infixl 2 :@

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

subst :: Symb -> Expr -> Expr -> Expr 
subst v n m = let
    freeN = freeVars n
    helper (Var s) linkM | s == v && not (s `elem` linkM) = n
                         | s /= v && s `elem` linkM && s `elem` freeN = Var $ s ++ "'"
                         | otherwise = Var s
    helper (a :@ b) linkM = helper a linkM :@ helper b linkM
    helper (Lam l e) linkM | v `elem` fLam && l `elem` freeN = Lam (l ++ "'") $ helper e (l:linkM)
                           | v `elem` fLam = Lam l $ helper e (l:linkM)
                           | otherwise = Lam l e
        where fLam = freeVars (Lam l e)
    in helper m []

freeVars :: Expr -> [Symb]
freeVars (Var x) = [x]
freeVars (n :@ m) = freeVars n `union` freeVars m
freeVars (Lam s e) = freeVars e \\ [s]