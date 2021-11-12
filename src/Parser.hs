module Parser where

import Debug.Trace
import Syntax
import Text.Parsec.Token qualified as Tok
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token qualified as Token

expr :: Parser Expr
expr =
  ( ifthen
      <|> mapfn
      <|> letin
      <|> try ternary
      <|> try lambda
      <|> list
      <|> formula
      <|> try fnAp
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
        [orOp],
        [pipeOp],
        [assignOp]
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
    assignOp = Infix (reservedOp "=" >> return assignExpr) AssocRight
    pipeOp = Infix (reservedOp "|>" >> return pipeExpr) AssocLeft

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
      Tok.reservedOpNames = ["in", "|>", "\\", "+", "*", "-", "=", "=="],
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

listContents :: Parser Expr
listContents = List <$> juxta `sepBy` many (space <|> char ',')

list :: Parser Expr
list = do
  char '['
  whitespace
  x <- try listContents
  char ']'
  return x

fnAp :: Parser Expr
fnAp = do
  -- _ <- trace ("calling f with x = ")
  a1 <- atom
  whitespace
  a2 <- expr
  return (App a1 a2)

mapfn :: Parser Expr
mapfn = do
  reserved "map"
  f <- (parens lambda) <|> atom
  xs <- expr
  return (LMap f xs)

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  -- FIXME: juxta borde funka här. för att kunda binda en funktion, t ex
  e1 <- atom
  reservedOp "in"
  e2 <- expr
  return (App (Lambda [x] e2) e1)

variable :: Parser Expr
variable = Atom `fmap` identifier

lambda :: Parser Expr
lambda = do
  xs <- identifier `sepBy` many space
  reservedOp ":"
  Lambda xs <$> expr

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- atom
  reservedOp "then"
  tr <- atom
  reserved "else"
  If cond tr <$> atom

ternary :: Parser Expr
ternary = do
  cond <- atom
  reservedOp "?"
  tr <- atom
  reserved ":"
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
