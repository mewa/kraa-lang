module Compiler
where

import Control.Monad.Trans.Class
import Text.Parsec as P
import Text.Parsec.String as P
import qualified Text.Parsec.Token as PT
import Data.Map.Strict as Map
import Control.Monad.State as S
import Control.Monad.Except
import Data.Functor.Identity

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
  symbols :: Map String (Either SExpr Compiled)
  } deriving (Show)

initialK = let map = insert "+" (Left (Atom $ Symbol "+")) Map.empty
  in KState map

data Expr = Kwd String
          | Ident String
          | Ui32Lit Int
          | StrLit String
          | Op String
          | ExprList [Expr]
          deriving (Show)

data Atom = Num Int
          | Str String
          | Symbol String
          deriving (Show)

data SExpr = Atom Atom | SExpr [SExpr]
  deriving (Show)

data Compiled = Lambda Int SExpr [Compiled] | Val Atom | Def String Compiled
  | CompiledSExpr [Compiled]
  deriving (Show)

compileTopLevel :: [SExpr] -> S.StateT KState (ExceptT String IO) [[Compiled]]
compileTopLevel exprs = do
  let compiled = (fmap compile exprs)
  sequence compiled

withLocalStateT local f = get >>= \st -> withStateT local f >>= \r -> put st >> return r

showSExpr :: SExpr -> String
showSExpr (Atom (Num e)) = show e
showSExpr (Atom (Str s)) = "\"" ++ s ++ "\""
showSExpr (Atom (Symbol s)) = s
showSExpr (SExpr exprs) = "(" ++ trim (concatMap (\s -> showSExpr s ++ " ") exprs) ++ ")"

trim = reverse . dropWhile (== ' ') . reverse

compile :: SExpr -> S.StateT KState (ExceptT String IO) [Compiled]
compile e@(SExpr (sargs@(SExpr args) : sbody@(SExpr body) : [])) = do
  let argsSymbolsOnly = all isSymbol args
        where
          isSymbol (Atom (Symbol _)) = True
          isSymbol _ = False
  if argsSymbolsOnly then do
    let updatedLocalState state = let symmap = symbols state
                                      addSyms = (\map (Atom (Symbol arg)) -> insert arg (Left sargs) map)
          in KState $ Prelude.foldl addSyms symmap argSymbols
        argSymbols = args
    let lambda b = return . return $ Lambda (length args) sargs b
    la <- withLocalStateT updatedLocalState $ compile sbody --(sequence $ fmap compile body)
    lambda la
  else
    throwError "only symbols are allowed as lambda arguments"
compile e@(Atom s@(Symbol sym)) = do
  checkSym sym
  return . return $ Val s
compile e@(Atom a@(Num _)) = do
  return . return $ Val a
compile e@(Atom a@(Str _)) = do
  return . return $ Val a
compile e@(SExpr [(Atom (Symbol "setq")), ((Atom (Symbol name))), r]) = do
  body <- compile r
  return . return $ Def name $ CompiledSExpr body
compile e@(SExpr s) = do
  b <- fmap concat $ sequence $ fmap compile s
  return . return $ CompiledSExpr b

checkSym :: (Monad m) => String -> S.StateT KState (ExceptT String m) ()
checkSym sym = do
  let notSetq = sym /= "setq"
  if notSetq then do
    s <- fmap (Map.lookup sym . symbols) get
    case s of
      Just _ -> return ()
      Nothing -> throwError $ "undefined symbol: " ++ sym
  else
    return ()

bf = runExceptT $ flip runStateT initialK $ do
  compileFile

compileFile :: S.StateT KState (ExceptT String IO) [[Compiled]]
compileFile = do
      a <- parseFile
      compileTopLevel a

sexpr :: Expr -> S.StateT KState (Either String) SExpr
sexpr (Ui32Lit n) = pure . Atom . Num $  n
sexpr (StrLit n) = pure . Atom . Str $  n
sexpr (Ident val) = pure . Atom . Symbol $ val
sexpr (Kwd "setq") = pure . Atom . Symbol $ "setq"
sexpr (Op val) = pure . Atom . Symbol $ val
sexpr (ExprList r@(Kwd "setq" : Ident name : _)) = do
  s <- get
  exprs <- sequence $ fmap sexpr r
  put . KState $ insert name (Left . SExpr $ tail exprs) (symbols s)
  pure $ SExpr exprs
sexpr (ExprList e)= sequence (fmap sexpr e) >>= return . SExpr
sexpr e = throwError $ "Invalid expression: " ++ show e

parseFile :: S.StateT KState (ExceptT String IO) [SExpr]
parseFile = do
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
  exprs <- parens $ many1 expr
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
  lookAhead $ many expr
  return $ Op op
