import Data.List
import Control.Monad.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

type Symb = String 

infixl 2 :@

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
              deriving Eq

instance Show Expr where
    showsPrec _ (Var x) = showString x
    showsPrec _ (Var x :@ Var y) = showString x . showString " " . showString y
    showsPrec _ (Var x :@ n) = showString (x ++ " (") . shows n . showString ")"
    showsPrec _ (n :@ Var x) = shows n . showString (" " ++ x)
    showsPrec _ (n :@ m) = showString "(" . shows n . showString ") (" . shows m . showString ")"
    showsPrec _ (Lam x (Lam y n)) = showString ("\\" ++ x ++ " ") . showLambda (Lam y n)
        where showLambda (Lam x (Lam y n)) = showString (x ++ " ") . showLambda (Lam y n)
              showLambda (Lam y n) = showString (y ++ " -> ") . shows n
    showsPrec _ (Lam x n) = showString ("\\" ++ x ++ " -> ") . shows n
    
languageDefinition :: GenLanguageDef String u Identity
languageDefinition =
       emptyDef { Token.nestedComments = False,
                     Token.caseSensitive = True,
                     Token.identStart = noneOf "\\-> ()\n\t",
                     Token.identLetter = noneOf "\\-> ()\n\t",
                     Token.reservedOpNames = [ "\\", "->"]
                   }

tokenParser = Token.makeTokenParser languageDefinition

identifier = Token.identifier tokenParser
reservedOp = Token.reservedOp tokenParser
parens = Token.parens tokenParser
whitespace = Token.whiteSpace tokenParser

some v = (:) <$> v <*> many v

instance Read Expr where
  readsPrec _ s =  [(parseExpr s, "")]

parseExpr s = case parse parser "" s of
    Left t -> error $ show t
    Right expr -> expr

parser :: Parser Expr
parser = parseApplication <|> parseAbstraction

parseApplication = do
    whitespace
    lstTerms <- some $ parseVars <|> parseParens
    whitespace
    return $ foldl1 (:@) lstTerms

parseAbstraction = do
    reservedOp "\\"
    argLst <- parseVarLst
    reservedOp "->"
    expr <- parser
    return $ foldr (\(Var x) y -> Lam x y) expr $ Var <$> argLst

parseParens = do
    term <- parens parser
    return term

parseVars = do
    lstVar <- parseVarLst
    return $ foldl1 (\x y -> x :@ y) $ Var <$> lstVar

parseVarLst = do
      lst <- some identifier
      return lst