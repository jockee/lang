module Parser where

import Syntax
import Text.Parsec.Token qualified as Tok
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token qualified as Token

expr :: Parser Expr
expr =
  ( ifthen
      <|> letin
      <|> try lambda
      <|> parseList
      <|> formula
  )
    <* whitespace <?> "expr"

formula :: Parser Expr
formula = whitespace >> buildExpressionParser table juxta <?> "expression"
  where
    table =
      [ [prefix "-" neg],
        [mulOp],
        [addOp, subOp],
        [eqOp],
        [andOp],
        [orOp]
      ]
    prefix name fun = Prefix (do reservedOp name; return fun)
    neg n = case n of
      LInteger x -> LInteger $ negate x
      LFloat x -> LFloat $ negate x
    eqOp = Infix (reservedOp "==" >> return eqExpr) AssocLeft
    subOp = Infix (reservedOp "-" >> return subExpr) AssocLeft
    addOp = Infix (reservedOp "+" >> return addExpr) AssocLeft
    mulOp = Infix (reservedOp "*" >> return mulExpr) AssocLeft
    andOp = Infix (reservedOp "&&" >> return andExpr) AssocLeft
    orOp = Infix (reservedOp "||" >> return orExpr) AssocLeft

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

whitespace :: Parser ()
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
juxta = foldl1 App <$> many1 atom

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
    "true" -> LBool True
    "false" -> LBool False
    _ -> Atom atom

parseListContents :: Parser Expr
parseListContents = List <$> juxta `sepBy` many (space <|> char ',')

parseList :: Parser Expr
parseList = do
  char '['
  whitespace
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
  return (App (Abs [x] e2) e1)

variable :: Parser Expr
variable = Atom `fmap` identifier

lambda :: Parser Expr
lambda = do
  x <- identifier
  reservedOp ":"
  Abs [x] <$> expr

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- atom
  reservedOp "then"
  tr <- atom
  reserved "else"
  If cond tr <$> atom

parseFloat :: Parser Expr
parseFloat = do
  whole <- many1 digit
  char '.'
  decimal <- many1 digit
  return $ LFloat (read (whole ++ "." ++ decimal))

parseInteger :: Parser Expr
parseInteger = do
  whole <- many1 digit
  return $ LInteger $ read whole

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
  return $ LString s

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  oneOf ['\\', '"']
