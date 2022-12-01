import Data.List

type Symb = String 
infixl 2 :@
infix 1 `alphaEq`

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

makeNew :: Symb -> Expr -> Symb
makeNew s e = helper s $ freeVars e
    where helper x l | x `elem` l = helper (x ++ "'") l
                     | otherwise = x

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var a) (Var b) = a == b
alphaEq (a :@ b) (n :@ m) = alphaEq a n && alphaEq b m
alphaEq (Lam x e1) (Lam y e2) = alphaEq (subst x (Var z) e1) (subst y (Var z) e2)
    where 
        z = makeNew x (e1 :@ e2)
alphaEq a b = False