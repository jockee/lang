module Parser where

import Control.Monad
import Data.List
import Debug.Trace
import Eval ()
import Exceptions
import Syntax
import Text.Parsec.Combinator (parserTraced)
import Text.Parsec.Error
import Text.Parsec.Token qualified as Tok
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token qualified as Token

expr :: Parser Expr
expr =
  whitespace
    >> ( lexeme
           ( ifthen
               <|> try typeDef
               <|> letin
               <|> try ternary
               <|> try lambda
               <|> try function
               <|> formula
           )
           <|> noop
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
        [pipeOp]
      ]
    prefix name fun = Prefix (do reservedOp name; return fun)
    neg n = case n of
      PInteger x -> PInteger $ negate x
      PFloat x -> PFloat $ negate x
    not' (PBool b) = PBool $ not b
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
      Tok.reservedOpNames = ["=>", ";", ":=", "in", "|>", "+", "++", "*", "-", "=", "==", "<", ">"],
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

brackets = Tok.brackets lexer

braces = Tok.brackets lexer

identifier :: Parser String
identifier = Token.identifier lexer <|> string "_"

juxta :: Parser Expr
juxta = foldl1 App <$> many1 term

term :: Parser Expr
term =
  lexeme
    ( try parseFloat
        <|> try (parens parseInternalFunction)
        <|> try parseInteger
        <|> parseMaybe
        <|> moduleAccess
        <|> dictAccess
        <|> try dict
        <|> try dictUpdate
        <|> try range
        <|> list
        <|> true
        <|> false
        <|> parseString
        <|> variable
        <|> try tuple
        <|> parens expr
    )
    <?> "term"

true :: Parser Expr
true = try $ string "true" >> return (PBool True)

false :: Parser Expr
false = try $ string "false" >> return (PBool False)

dictContents :: Parser Expr
dictContents = do
  pairs <- pair `sepBy` many (space <|> char ',')
  return (PDict (sig [AnyType] AnyType) pairs)
  where
    pair = do
      key <- try (dictKey <* string ":") <|> (variable <* string "=>")
      whitespace
      val <- expr
      return (key, val)

dict :: Parser Expr
dict = do
  char '{'
  whitespace
  x <- try $ dictContents
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
  whitespace
  updates <- try dictContents <|> variable
  char '}'
  optional $ many (space <|> (char '}'))
  return (PDictUpdate dct updates) -- NOTE: could probably be converted to 'App' of stdlib `#merge` function when it exists

dictAccess :: Parser Expr
dictAccess = dotKey <|> try dictDotKey
  where
    dotKey = do
      string "."
      x <- dictKey
      dct <- variable <|> dict
      return (DictAccess x dct)
    dictDotKey = do
      first <- lower
      rest <- option [] identifier
      char '.'
      x <- dictKey
      return (DictAccess x (Atom (sig [DictionaryType] (DictionaryType)) (first : rest)))

tupleContents :: Parser [Expr]
tupleContents = (juxta <|> formula) `sepBy2` many (space <|> char ',')

sepBy2 p sep = liftM2 (:) p (many1 (sep >> p))

listContents :: Parser [Expr]
listContents = (juxta <|> formula) `sepBy` many (space <|> char ',')

range :: Parser Expr
range = do
  char '['
  whitespace
  lBound <- term
  string ".."
  whitespace
  uBound <- term
  char ']'
  return (PRange (sig [AnyType] AnyType) lBound uBound)

tuple :: Parser Expr
tuple = do
  char '('
  whitespace
  x <- try tupleContents
  char ')'
  return (PTuple (sig [ListType AnyType] (ListType AnyType)) x)

list :: Parser Expr
list = do
  char '['
  whitespace
  x <- try listContents
  char ']'
  return (PList (sig [ListType AnyType] (ListType AnyType)) x)

parseInternalFunction :: Parser Expr
parseInternalFunction = do
  reserved "InternalFunction"
  whitespace
  f <- identifier
  args <- list <|> variable
  return (InternalFunction f args)

-- caseof :: Parser Expr
-- caseof = do
--   reserved "case"
--   e <- expr
--   reservedOp "of"
--   e1 <- term
--   whitespace
--   reservedOp "in"
--   e2 <- expr
--   return (App (Lambda (sig [AnyType] AnyType) [x] e2) e1)

parseModule :: Parser Expr
parseModule = do
  reserved "module"
  first <- upper
  rest <- many (letter <|> digit)
  optional whitespace
  reservedOp "{"
  contents <- manyExpressions
  reservedOp "}"
  return (Module (first : rest) contents)

letin :: Parser Expr
letin = do
  reserved "let"
  x <- variable
  reservedOp "="
  e1 <- term
  whitespace
  reservedOp "in"
  e2 <- expr
  return (App (Lambda (sig [AnyType] AnyType) [x] e2) e1)

moduleAccess :: Parser Expr
moduleAccess = do
  first <- upper
  rest <- option [] identifier
  char '.'
  x <- identifier
  return (Atom (sig [AnyType] (AnyType)) ([first] ++ rest ++ "." ++ x))

variable :: Parser Expr
variable = Atom (sig [] AnyType) `fmap` identifier

dictKey :: Parser Expr
dictKey = PDictKey `fmap` identifier

typeDef :: Parser Expr
typeDef = do
  name <- identifier
  reservedOp "::"
  bindings <- identifier `sepBy1` reservedOp "->"
  let args = case init bindings of
        [] -> []
        ["TakesAnyArgsType"] -> [TakesAnyArgsType]
        ts ->
          map
            ( \case
                "Any" -> AnyType
                t -> toLangType t
            )
            ts
  let rtrn = case last bindings of
        "Any" -> AnyType
        s -> toLangType $ last bindings
  return (NamedTypeSig TypeSig {typeSigName = Just name, typeSigIn = args, typeSigReturn = rtrn})

function :: Parser Expr
function = do
  terms <- term `sepBy1` many space
  reservedOp "="
  let (name : args) = terms
  body <- expr
  let Atom _ nameStr = name
  let funSig = TypeSig {typeSigName = Just nameStr, typeSigIn = [], typeSigReturn = AnyType}
  if null args
    then return $ Binop Assign name body
    else case name of
      (Atom _ _) -> return $ Binop Assign name (Lambda funSig args body)
      destructureObject -> return $ Binop Assign destructureObject body

lambda :: Parser Expr
lambda = do
  identifiers <- (variable <|> tuple) `sepBy` many space
  reservedOp ":"
  body <- expr
  return (Lambda (sig [AnyType] AnyType) identifiers body)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- term
  reservedOp "then"
  tr <- term
  reserved "else"
  PIf cond tr <$> term

ternary :: Parser Expr
ternary = do
  cond <- term
  reservedOp "?"
  tr <- term
  reserved ":"
  PIf cond tr <$> term

parseMaybe :: Parser Expr
parseMaybe = nothing <|> just
  where
    nothing = do
      string "Nothing"
      return PNothing
    just = do
      string "Just"
      whitespace
      justVal <- expr
      return $ PJust (sig [AnyType] AnyType) justVal

parseFloat :: Parser Expr
parseFloat = do
  whole <- many1 digit
  char '.'
  decimal <- many1 digit
  return $ PFloat (read (whole ++ "." ++ decimal))

parseInteger :: Parser Expr
parseInteger = do
  whole <- many1 digit
  return $ PInteger $ read whole

noop :: Parser Expr
noop = do
  lexeme eof
  return PNoop

allOf :: Parser a -> Parser a
allOf p =
  do
    r <- p
    eof
    return r
    <?> "EOF"

parseExpr :: String -> Expr
parseExpr e = head $ parseExprs e

parseExprs :: String -> [Expr]
parseExprs s = case parseExprs' s of
  Left err -> error $ show err
  Right exprs -> exprs

manyExpressions :: Parser [Expr]
manyExpressions = (parseModule <|> expr) `sepBy` (char ';')

parseExprs' :: String -> Either ParseError [Expr]
-- parseExprs' = parse (allOf (manyExpressions) "stdin"
parseExprs' = parse (allOf (manyExpressions)) "stdin"

parseString :: Parser Expr
parseString = do
  char '"'
  s <- many (escapedChars <|> noneOf ['\\', '"'])
  char '"'
  return $ PString s

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  oneOf ['\\', '"']

sig :: [LangType] -> LangType -> TypeSig
sig inn out = TypeSig {typeSigName = Nothing, typeSigIn = inn, typeSigReturn = out}
