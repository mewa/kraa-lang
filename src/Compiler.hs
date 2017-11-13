module Compiler
where

import Control.Monad.Trans.Class
import Text.Parsec as P
import Text.Parsec.String as P
import qualified Text.Parsec.Token as PT
import Data.Map.Strict as Map
import Control.Monad.State as S
import Control.Monad.Except
import Control.Monad.Identity

kraaLang :: PT.LanguageDef st
kraaLang = PT.LanguageDef "" "" "" True
  (letter) -- identStart
  (alphaNum) -- identLetter
  (oneOf "=+-*/") -- opStart
  (oneOf "") -- opLetter
  ["setq"] [] False

lexer = PT.makeTokenParser kraaLang
parens = PT.parens lexer
identifier = PT.identifier lexer
operator = PT.operator lexer
integer = PT.integer lexer
stringLiteral = PT.stringLiteral lexer
reserved = PT.reserved lexer

data KState = KState {
  symbols :: Map String (Maybe SExpr)
  } deriving (Show)

initialK = KState Map.empty

data Expr = Kwd String
          | Ident String
          | Ui32Lit Int
          | StrLit String
          | Op String [Expr]
          | ExprList [Expr]
          deriving (Show)

data Atom = Num Int
          | Str String
          | Symbol String
          deriving (Show)

data SExpr = Atom Atom | SExpr [SExpr]
  deriving (Show)

sexpr :: Expr -> S.StateT KState (Either String) SExpr
sexpr (Ui32Lit n) = pure . Atom . Num $  n
sexpr (StrLit n) = pure . Atom . Str $  n
sexpr (Ident val) = pure . Atom . Symbol $ val
sexpr (Op val exprs) = do
  sexprs <- sequence $ fmap sexpr exprs
  let op = (Atom . Symbol $ val) : sexprs
  return $ SExpr op
sexpr (ExprList (Kwd "setq" : r@(Ident name : _))) = do
  s <- get
  exprs <- sequence $ fmap sexpr r
  put . KState $ insert name (Just . SExpr $ tail exprs) (symbols s)
  pure $ SExpr exprs
sexpr (ExprList e)= sequence (fmap sexpr e) >>= return . SExpr
sexpr e = throwError $ "Invalid expression: " ++ show e

compile :: S.StateT KState (ExceptT String IO) [SExpr]
compile = do
  f <- liftIO $ readFile "kr/test.kr"
  symbols <- lift $ ExceptT . return $ (getSymbols $ f)
  mapM toExceptT $ fmap sexpr symbols

toExceptT :: (Monad m) =>
  S.StateT KState (Either String) SExpr -> S.StateT KState (ExceptT String m) SExpr
toExceptT (StateT f) = S.StateT $ ExceptT . return . f

getSymbols :: String -> (Either String [Expr])
getSymbols s = case parse (many1 expr) "error" s of
  Left e -> Left $ show e
  Right e -> Right e

expr :: Parser Expr
expr = do
  try paren
    <|> try keyword
    <|> try ident
    <|> try op
    <|> try str
    <|> try num

paren :: Parser Expr
paren = do
  exprs <- parens $ many expr
  return $ ExprList exprs

keyword :: Parser Expr
keyword = do
  kwd <- reserved "setq"
  return $ Kwd "setq"

ident :: Parser Expr
ident = fmap Ident identifier

num :: Parser Expr
num = fmap (Ui32Lit . fromInteger) $ integer

str :: Parser Expr
str = fmap StrLit stringLiteral

op :: Parser Expr
op = do
  op <- operator
  exprs <- many expr
  return $ Op op exprs
