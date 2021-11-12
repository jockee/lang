module Parser where

import Control.Monad
import Data.Functor.Identity
import Numeric
import Syntax
import Text.Parsec.Expr qualified as Ex
import Text.Parsec.Token qualified as Tok
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token qualified as Token

expr :: Parser Expr
expr =
  ( ifthen
      <|> letin
      <|> lambda
      <|> parseList
      <|> formula
  )
    <* whitespace <?> "expr"

formula :: Parser Expr
formula = whitespace >> (buildExpressionParser table juxta) <?> "expression"
  where
    table =
      [ [prefix "-" neg],
        [mulOp],
        [addOp, subOp],
        [eqOp]
      ]
    prefix name fun = Prefix (do reservedOp name; return fun)
    neg n = case n of
      Integer x -> Integer $ negate x
      Float x -> Float $ negate x
    eqOp = Infix (reservedOp "==" >> return eqExpr) AssocLeft
    subOp = Infix (reservedOp "-" >> return subExpr) AssocLeft
    addOp = Infix (reservedOp "+" >> return addExpr) AssocLeft
    mulOp = Infix (reservedOp "*" >> return mulExpr) AssocLeft

-- [binary "==" eq],
-- [binary "&&" and],
-- [binary "||" or]
-- binary c op = Infix (op <$ (whitespace >> string c >> whitespace)) AssocLeft
-- eq v1 v2 = Bool $ v1 == v2
-- and (Bool x) (Bool y) = Bool $ x && y
-- or (Bool x) (Bool y) = Bool $ x || y
langDef :: Tok.LanguageDef ()
langDef =
  Tok.LanguageDef
    { Tok.commentStart = "{-",
      Tok.commentEnd = "-}",
      Tok.commentLine = "//",
      Tok.nestedComments = True,
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_'",
      Tok.opStart = oneOf "",
      Tok.opLetter = oneOf "",
      Tok.reservedNames = [],
      Tok.reservedOpNames = ["in", "->", "\\", "+", "*", "-", "="],
      Tok.caseSensitive = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whitespace = Tok.whiteSpace lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

juxta :: Parser Expr
juxta = (foldl1 App) `fmap` (many1 atom)

atom :: Parser Expr
atom =
  ( try parseFloat
      <|> try parseInteger
      <|> parseAtom
      <|> parseString
      <|> variable
      <|> parens expr
  )
    <* whitespace <?> "atom"

parseAtom :: Parser Expr
parseAtom = do
  first <- letter
  rest <- many (letter <|> digit)
  let atom = first : rest
  return $ case atom of
    "true" -> Bool True
    "false" -> Bool False
    _ -> Atom atom

parseListContents :: Parser Expr
parseListContents = List <$> juxta `sepBy` many (space <|> char ',')

parseList :: Parser Expr
parseList = do
  char '['
  x <- try parseListContents
  char ']'
  return x

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  -- FIXME: juxta borde funka här. atom gör det, dock
  e1 <- atom
  reservedOp "in"
  e2 <- expr
  return (App (Abs x e2) e1)

variable :: Parser Expr
variable = Var `fmap` identifier

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp "->"
  e <- expr
  return (Abs x e)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- atom
  reservedOp "then"
  tr <- atom
  reserved "else"
  e <- atom
  return (If cond tr e)

parseFloat :: Parser Expr
parseFloat = do
  whole <- many1 digit
  char '.'
  decimal <- many1 digit
  return $ Float (read (whole ++ "." ++ decimal))

parseInteger :: Parser Expr
parseInteger = do
  whole <- many1 digit
  return $ Integer $ read (whole)

allOf :: Parser a -> Parser a
allOf p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Expr
parseExpr t =
  case parse (allOf expr) "stdin" t of
    Left err -> error (show err)
    Right ast -> ast

parseString :: Parser Expr
parseString = do
  char '"'
  s <- many (escapedChars <|> noneOf ['\\', '"'])
  char '"'
  return $ String s

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  oneOf ['\\', '"']
