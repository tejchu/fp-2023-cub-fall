
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import Syntax
import TypeCheck
import Error
-- parser :: "...." -> AST
-- parser combinators ::  "prefix .. postfix" -> (AST, " postfix" )

type Parser = Parsec Void Text




sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

dot :: Parser Text
dot = symbol "." <?> "dot"

colon :: Parser Text
colon = symbol ":" <?> "colon"

kw :: Text -> Parser Text
kw keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

true :: Parser Bool
true = kw "True" *> return True <?> "True"

false :: Parser Bool
false = kw "False" *> return False <?> "False"

ifKW :: Parser Text
ifKW = kw "If" <?> "If"

-- add "let" for >let< x = term in term
letKW :: Parser Text
letKW = kw "Let" <?> "Let"

-- add "in" for let x = term >in< term //equals added lower
inKW :: Parser Text
inKW = kw "in" <?> "in"

thenKW :: Parser Text
thenKW = kw "Then" <?> "then"

elseKW :: Parser Text
elseKW = kw "Else" <?> "Else"

boolKW :: Parser Text
boolKW = kw "Bool" <?> "Bool"

-- add intKW for int type
intKW :: Parser Text
intKW = kw "Int" <?> "Int"

-- abstraction :: λ x : T . b

lambda :: Parser Text
lambda = symbol "λ" <|> symbol "\\" <?> "lambda symbol"

ident :: Parser String
ident = lexeme ((:) <$> lowerChar <*> many alphaNumChar <?> "variable")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- add equals for let x >=< term in term
equals :: Parser Text
equals = symbol "=" <?> "equal sign"

-- add the ops
addOp :: [[Operator Parser (Term String)]]
addOp = [[binary "+" Add, binary "-" Sub]]
  where
    binary name f = InfixL (f <$ symbol name)

mulOp :: [[Operator Parser (Term String)]]
mulOp = [[binary "*" Mul, binary "/" Div]]
  where
    binary name f = InfixL (f <$ symbol name)
-- not sure if these are correct yet

pLambdaTerm :: Parser (Term String)
pLambdaTerm = choice
  [ pVar <?> "variable error"
  , pAbstraction <?> "abstraction error"
  , pApplication <?> "application error"
  , pBoolLit <?> "boolean literal error"
  , pIntLit <?> "integer literal error"
  , pIf <?> "if expression error"
  , pLet <?> "let binding error"
  ]
  <?> "lambda term"

pVar :: Parser (Term String)
pVar =
  Var <$> ident <?> "variable"

pAbstraction :: Parser (Term String)
pAbstraction =
  Abs <$> (lambda *> ident) <*> (colon *> pType) <*> (dot *> pLambdaTerm)
  <?> "abstraction"

pApplication :: Parser (Term String)
pApplication =
  foldl1 App <$> some (parens pLambdaTerm)
  <?> "application"

pBoolLit :: Parser (Term String)
pBoolLit =
  BoolLit <$> (true <|> false)
  <?> "boolean literal"

-- add parser for integer literals
pIntLit :: Parser (Term String)
pIntLit = 
  IntLit <$> integer 
  <?> "integer literal"

pIf :: Parser (Term String)
pIf =
  If <$> (ifKW *> pLambdaTerm) <*> (thenKW *> pLambdaTerm) <*> (elseKW *> pLambdaTerm)
  <?> "if expression"

-- let binding
pLet :: Parser (Term String)
pLet = do
--Let (x) = term (in) term
  Let <$> (letKW *> ident) <*> (equals *> pLambdaTerm) <*> (inKW *> pLambdaTerm)
  <?> "let binding"

-- add parsers for arithmetic
pAdd :: Parser (Term String)
pAdd = 
  Add <$> (pLambdaTerm <* symbol "+") <*> pLambdaTerm
  <?> "addition"

pSub :: Parser (Term String)
pSub = 
  Sub <$> (pLambdaTerm <* symbol "-") <*> pLambdaTerm
  <?> "subtraction"

pMul :: Parser (Term String)
pMul = 
  Mul <$> (pLambdaTerm <* symbol "*") <*> pLambdaTerm
  <?> "multiplication"

pDiv :: Parser (Term String)
pDiv = 
  Div <$> (pLambdaTerm <* symbol "/") <*> pLambdaTerm
  <?> "division"


pType :: Parser Type
pType =
    makeExprParser pBaseType [[binary "->" Arrow]]
    <?> "arrow"
  where
    binary name f = InfixR (f <$ symbol name)

-- update base type parser to include int type
pBaseType :: Parser Type
pBaseType = pTyVar <|> pBoolType <|> pIntType <|> parens pType <?> "base type"

pTyVar :: Parser Type
pTyVar = TyVar <$> ident <?> "type variable"

pBoolType :: Parser Type
pBoolType = Bool <$ boolKW <?> "Bool type tag"

-- add parser type for ints
pIntType :: Parser Type
pIntType = Int <$ intKW <?> "Int type tag"

parseEof :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parseEof p = runParser (p <* eof) ""

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

-- I am not sure where else to expose the error
parseLambdaTerm :: Text -> Either LambdaError (Term String)
parseLambdaTerm input =
  case parseEof pLambdaTerm input of
    Left err -> Left $ ParseError (errorBundlePretty err)
    Right term -> Right term

