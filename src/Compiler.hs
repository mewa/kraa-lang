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

data IRExpr = IRExpr {
  name :: String
  , irExpr :: String
  , irArity :: Int
  , exprType :: ExprType
  } deriving (Show)

data ExprType = Unit | Int | String | Ptr | Morph [ExprType]
  deriving (Show, Eq)

arity :: ExprType -> Int
arity (Morph t) = max 0 $ length t - 1
arity _ = 0

data TypedExpr = TypedExpr Compiled ExprType | InferredExpr Compiled | TypedExprList [TypedExpr] ExprType
  deriving (Show)

boundExpr (InferredExpr _) = Nothing
boundExpr e = Just $ boundType e

boundType (TypedExpr _ t) = t
boundType (TypedExprList _ t) = t

irAdd = IRExpr "add" "add" 2 $ Morph [Int, Int, Int]

data KState = KState {
  symbols :: Map String Symbol
  , genSym :: Int
  } deriving (Show)

data Symbol = Parsed SExpr | TypeChecked TypedExpr | IR IRExpr
  deriving (Show)

typeCheck :: Compiled -> StateT KState (ExceptT String Identity) TypedExpr
typeCheck e@(Val (Symbol s)) = do
  name <- gets (Map.lookup s . symbols)
  case name of
    Nothing -> pure $ InferredExpr e
    Just (Parsed sexpr) -> throwError $ "uncompiled expr: " ++ showSExpr sexpr
    Just (TypeChecked expr) -> pure expr
    Just (IR (IRExpr _ _ _ t)) -> pure $ TypedExpr e t
typeCheck e@(Val (Num n)) = pure $ TypedExpr e Int
typeCheck e@(Val (Str s)) = pure $ TypedExpr e String
typeCheck e@(Lambda n (CompiledSExpr args) body) = do
  s <- get
  let stateWithArgs = s { symbols = Prelude.foldl addArgs (symbols s) args }
        where
          addArgs map e@(Val (Symbol arg)) = Map.insert arg (TypeChecked (InferredExpr e)) map
  types <- withLocalStateT (const stateWithArgs) $ mapM typeCheck body
  let (TypedExprList _ t) = last types
  return $ TypedExpr e t
typeCheck e@(Def name expr) = typeCheck expr
typeCheck e@(CompiledSExpr exprs) = do
  texprs <- sequence $ fmap typeCheck exprs
  reduceTypedExprs texprs

reduceTypedExprs :: [TypedExpr] ->
  StateT KState (ExceptT String Identity) TypedExpr
reduceTypedExprs exprs = do
  (exprType, exprs) <- foldM foldTypedExprs (Unit, []) (reverse exprs)
  return $ TypedExprList exprs exprType

foldTypedExprs :: (ExprType, [TypedExpr]) -> TypedExpr ->
  StateT KState (ExceptT String Identity) (ExprType, [TypedExpr])
foldTypedExprs (stTypes, st) e@(TypedExpr ex t) = do
  let ar = arity t
      stlen = length st
      maxAr = if ar > stlen then stlen else ar
  case ar > 0 of
    False -> return $ (Unit, e : st)
    True -> do
      let exprList = e : newExprs
          (Morph types) = t
          (matching, newTypes) = matchTypes types newExprs
          newType = case drop maxAr types of
                      t@(x:y:z) -> Morph t
                      [t] -> t
          newExprs = take maxAr st

      if matching then
        return $ (newType, exprList ++ (drop maxAr st))
        else
        throwError $ "type err: " ++ show (types, newTypes) ++ "\n" ++ show st
foldTypedExprs (t, st) e = return $ (Unit, e : st)

matchTypes :: [ExprType] -> [TypedExpr] -> (Bool, [(ExprType, ExprType)])
matchTypes types exprs = let
  match = Prelude.foldl (\acc (a, b) -> acc && a == b) True matchedTypes
  matchedTypes = zipWith matchType types exprs
  in (match, matchedTypes)
    where
      matchType :: ExprType -> TypedExpr -> (ExprType, ExprType)
      matchType t e = case boundExpr e of
        Just et -> (t, et)
        Nothing -> (t, t)

initialK = let map = insert "+" (IR irAdd) Map.empty
  in KState map 0

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

data Compiled = Lambda Int Compiled [Compiled] | Val Atom | Def String Compiled
  | CompiledSExpr [Compiled]
  deriving (Show)

compileTopLevel :: (Monad m) => [SExpr] -> S.StateT KState (ExceptT String m) [[Compiled]]
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

compile :: (Monad m) => SExpr -> S.StateT KState (ExceptT String m) [Compiled]
compile e@(SExpr (sargs@(SExpr args) : sbody@(SExpr body) : [])) = do
  let argsSymbolsOnly = all isSymbol args
        where
          isSymbol (Atom (Symbol _)) = True
          isSymbol _ = False
  if argsSymbolsOnly then do
    let updatedLocalState state = let symmap = symbols state
                                      addSyms = (\map (Atom (Symbol arg)) -> insert arg (Parsed sargs) map)
          in state { symbols = (Prelude.foldl addSyms symmap argSymbols) }
        argSymbols = args
    compiledArgs <- withLocalStateT updatedLocalState $ compile sargs
    let lambda b = return . return $ Lambda (length args) (head compiledArgs) b
    la <- withLocalStateT updatedLocalState $ compile sbody
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

compileFile :: IO () -- (Either String [[TypedExpr]])
compileFile = do
  f <- parseFile
  let typed = runIdentity . runExceptT $ flip evalStateT initialK $ do
        parsed <- lift . ExceptT $ return $  getSymbols f
        sexprs <- mapM (toExceptT) $ fmap sexpr parsed
        compiled <- compileTopLevel sexprs
        mapM (mapM typeCheck) compiled
  case typed of
    Left err -> putStrLn err
    Right exprs -> print exprs

sexpr :: Expr -> S.StateT KState (Either String) SExpr
sexpr (Ui32Lit n) = pure . Atom . Num $  n
sexpr (StrLit n) = pure . Atom . Str $  n
sexpr (Ident val) = pure . Atom . Symbol $ val
sexpr (Kwd "setq") = pure . Atom . Symbol $ "setq"
sexpr (Op val) = pure . Atom . Symbol $ val
sexpr (ExprList r@(Kwd "setq" : Ident name : _)) = do
  s <- get
  exprs <- sequence $ fmap sexpr r
  put $ s { symbols = insert name (Parsed . SExpr $ tail exprs) (symbols s) }
  pure $ SExpr exprs
sexpr (ExprList e)= sequence (fmap sexpr e) >>= return . SExpr
sexpr e = throwError $ "Invalid expression: " ++ show e

parseFile :: IO String
parseFile = readFile "kr/test.kr"

toExceptT :: (Monad m) =>
  S.StateT KState (Either String) SExpr -> S.StateT KState (ExceptT String m) SExpr
toExceptT (StateT f) = S.StateT $ ExceptT . return . f

getSymbols :: String -> (Either String [Expr])
getSymbols s = case parse (many1 paren) "error" s of
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
