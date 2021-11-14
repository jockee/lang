module Parser where

import Debug.Trace
import Syntax
import Text.Parsec.Token qualified as Tok
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token qualified as Token

expr :: Parser Expr
expr =
  whitespace
    >> ( ifthen
           <|> lFold
           -- <|> mapfn
           <|> letin
           <|> try ternary
           <|> try lambda
           <|> formula
       )
    <* whitespace <?> "expr"

formula :: Parser Expr
formula = whitespace >> buildExpressionParser table juxta <?> "expression"
  where
    table =
      [ [prefix "-" neg, prefix "!" not'],
        [mulOp],
        [addOp, subOp],
        [eqOp],
        [andOp],
        [orOp],
        [concatOp],
        [pipeOp],
        [assignOp]
      ]
    prefix name fun = Prefix (do reservedOp name; return fun)
    neg n = case n of
      LInteger x -> LInteger $ negate x
      LFloat x -> LFloat $ negate x
    not' (LBool b) = LBool $ not b
    eqOp = Infix (reservedOp "==" >> return (Binop Eql)) AssocLeft
    subOp = Infix (reservedOp "-" >> return (Binop Sub)) AssocLeft
    addOp = Infix (try $ reservedOp "+" <* notFollowedBy (char '+') >> return (Binop Add)) AssocLeft
    mulOp = Infix (reservedOp "*" >> return (Binop Mul)) AssocLeft
    andOp = Infix (reservedOp "&&" >> return (Binop And)) AssocLeft
    orOp = Infix (reservedOp "||" >> return (Binop Or)) AssocLeft
    concatOp = Infix (reservedOp "++" >> return (Binop Concat)) AssocLeft
    assignOp = Infix (reservedOp "=" >> return (Binop Assign)) AssocRight
    pipeOp = Infix (reservedOp "|>" >> return (Binop Pipe)) AssocLeft

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
      Tok.reservedOpNames = ["in", "|>", "\\", "+", "++", "*", "-", "=", "=="],
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
juxta = foldl1 App <$> many1 term

term :: Parser Expr
term =
  ( try parseFloat
      <|> try parseInteger
      <|> list
      <|> parseAtom
      <|> parseString
      <|> variable
      <|> parens expr
  )
    <* whitespace <?> "term"

parseAtom :: Parser Expr
parseAtom = do
  first <- letter
  rest <- many (letter <|> digit)
  let term = first : rest
  return $ case term of
    "true" -> LBool True
    "false" -> LBool False
    _ -> Atom term

listContents :: Parser Expr
listContents = List <$> juxta `sepBy` many (space <|> char ',')

lista :: Parser Expr
lista = do
  string "List(1)"
  return (List [LInteger 1])

list :: Parser Expr
list = do
  char '['
  whitespace
  x <- try listContents
  char ']'
  return x

lFold :: Parser Expr
lFold = do
  reserved "foldInternal"
  f <- parens lambda <|> variable
  initValue <- term
  xs <- list <|> term
  return (LFold f initValue xs)

-- mapfn :: Parser Expr
-- mapfn = do
--   reserved "map"
--   f <- parens lambda <|> term
--   LMap f <$> expr

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- term
  whitespace
  reservedOp "in"
  e2 <- expr
  return (App (Lambda [x] e2) e1)

variable :: Parser Expr
variable = Atom `fmap` identifier

lambda :: Parser Expr
lambda = do
  identifiers <- identifier `sepBy` many space
  reservedOp ":"
  Lambda identifiers <$> expr

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- term
  reservedOp "then"
  tr <- term
  reserved "else"
  If cond tr <$> term

ternary :: Parser Expr
ternary = do
  cond <- term
  reservedOp "?"
  tr <- term
  reserved ":"
  If cond tr <$> term

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
  r <- p
  eof
  return r

parseExpr :: String -> Expr
parseExpr t =
  case parse (many expr) "stdin" t of
    Left err -> error (show err)
    Right exprs -> foldl1 App exprs

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
