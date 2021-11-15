module Parser where

import Debug.Trace
import Exceptions
import Syntax
import Text.Parsec.Error
import Text.Parsec.Token qualified as Tok
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token qualified as Token

expr :: Parser Expr
expr =
  lexeme
    ( ifthen
        <|> try function
        <|> lFold
        <|> letin
        <|> try ternary
        <|> try lambda
        <|> formula
    )
    <?> "expr"

formula :: Parser Expr
formula = buildExpressionParser table juxta <?> "formula"
  where
    table =
      [ [prefix "-" neg, prefix "!" not'],
        [mulOp],
        [addOp, subOp],
        [eqOp, notEqOp],
        [andOp],
        [orOp],
        [gteOp, lteOp, gtOp, ltOp],
        [concatOp],
        [pipeOp],
        [assignOp]
      ]
    prefix name fun = Prefix (do reservedOp name; return fun)
    neg n = case n of
      LInteger x -> LInteger $ negate x
      LFloat x -> LFloat $ negate x
    not' (LBool b) = LBool $ not b
    notEqOp = Infix (reservedOp "!=" >> return (Binop NotEql)) AssocLeft
    eqOp = Infix (reservedOp "==" >> return (Binop Eql)) AssocLeft
    subOp = Infix (reservedOp "-" >> return (Binop Sub)) AssocLeft
    addOp = Infix (try $ reservedOp "+" <* notFollowedBy (char '+') >> return (Binop Add)) AssocLeft
    mulOp = Infix (reservedOp "*" >> return (Binop Mul)) AssocLeft
    andOp = Infix (reservedOp "&&" >> return (Binop And)) AssocLeft
    gtOp = Infix (reservedOp ">" >> return (Cmp ">")) AssocLeft
    ltOp = Infix (reservedOp "<" >> return (Cmp "<")) AssocLeft
    gteOp = Infix (reservedOp ">=" >> return (Cmp ">=")) AssocLeft
    lteOp = Infix (reservedOp "<=" >> return (Cmp "<=")) AssocLeft
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
      Tok.identLetter = alphaNum <|> oneOf "_'?",
      Tok.opStart = oneOf "",
      Tok.opLetter = oneOf "",
      Tok.reservedNames = [],
      Tok.reservedOpNames = [":=", "in", "|>", "+", "++", "*", "-", "=", "==", "<", ">"],
      Tok.caseSensitive = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

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
  lexeme
    ( try parseFloat
        <|> try parseInteger
        <|> dictAccess
        <|> try dict
        <|> try dictUpdate
        <|> list
        <|> true
        <|> false
        <|> parseString
        <|> variable
        <|> parens expr
    )
    <?> "term"

true :: Parser Expr
true = try $ string "true" >> return (LBool True)

false :: Parser Expr
false = try $ string "false" >> return (LBool False)

dictContents :: Parser Expr
dictContents = do
  pairs <- pair `sepBy` many (space <|> char ',')
  return (Dict pairs)
  where
    pair = do
      key <- dictKey
      char ':'
      whitespace
      val <- expr
      return (key, val)

dict :: Parser Expr
dict = do
  char '{'
  whitespace
  x <- try dictContents
  char '}'
  return x

dictUpdate :: Parser Expr
dictUpdate = do
  char '{'
  whitespace
  dct <- variable <|> dict
  whitespace
  char '|'
  whitespace
  optional $ char '{'
  updates <- try dictContents
  char '}'
  optional $ many (space <|> (char '}'))
  return (DictUpdate dct updates) -- NOTE: could probably be converted to 'App' of stdlib `#merge` function when it exists

dictAccess :: Parser Expr
dictAccess = dotKey <|> try dictDotKey
  where
    dotKey = do
      string "."
      x <- dictKey
      dct <- variable <|> dict
      return (DictAccess x dct)
    dictDotKey = do
      dct <- variable <|> dict
      char '.'
      x <- dictKey
      return (DictAccess x dct)

listContents :: Parser Expr
listContents = List <$> juxta `sepBy` many (space <|> char ',')

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

dictKey :: Parser Expr
dictKey = DictKey `fmap` identifier

function :: Parser Expr
function = do
  bindings <- identifier `sepBy` many space
  reservedOp ":="
  body <- expr
  let (name : args) = bindings
  return $ Binop Assign (Atom name) (Lambda args body)

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

noop :: Parser Expr
noop = do
  whitespace
  lexeme eof
  return Noop

allOf :: Parser a -> Parser a
allOf p =
  do
    whitespace
    r <- p
    eof
    return r
    <?> "EOF"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
  Left err -> error $ show err
  Right expr -> expr

parseExpr' :: String -> Either ParseError Expr
parseExpr' s = parse (try noop <|> allOf expr) "stdin" s

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
