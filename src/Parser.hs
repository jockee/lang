module Parser where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Void
import Debug.Trace
import Eval ()
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

sc :: Parser ()
sc = L.space hspace1 lineComment blockComment

expr :: Parser Expr
expr =
  comment <|> sc
    *> ( comment
           <|> ifthen
           <|> try typeDef
           <|> letin
           <|> try lambda
           <|> try ternary
           <|> try function
           <|> formula
           <|> noop
       )
    <?> "expr"

formula :: Parser Expr
formula = makeExprParser juxta table <?> "formula"
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
    prefix name fun = Prefix (do string name; return fun)
    neg n = case n of
      PInteger x -> PInteger $ negate x
      PFloat x -> PFloat $ negate x
    not' (PBool b) = PBool $ not b
    notEqOp = InfixL (string "!=" <* optional sc >> return (Binop NotEql))
    eqOp = InfixR (string "==" <* optional sc >> return (Binop Eql))
    subOp = InfixL (string "-" <* optional sc >> return (Binop Sub))
    addOp = InfixL (try $ string "+" <* notFollowedBy (char '+') <* optional sc >> return (Binop Add))
    mulOp = InfixL (string "*" <* optional sc >> return (Binop Mul))
    andOp = InfixL (string "&&" <* optional sc >> return (Binop And))
    gtOp = InfixL (string ">" <* optional sc >> return (Cmp ">"))
    ltOp = InfixL (string "<" <* optional sc >> return (Cmp "<"))
    gteOp = InfixL (string ">=" <* optional sc >> return (Cmp ">="))
    lteOp = InfixL (string "<=" <* optional sc >> return (Cmp "<="))
    orOp = InfixL (string "||" <* optional sc >> return (Binop Or))
    concatOp = InfixL (string "++" <* optional sc >> return (Binop Concat))
    pipeOp = InfixL (string "|>" <* optional sc >> return (Binop Pipe))

lexeme = L.lexeme hspace

symbol = L.symbol hspace

parens = between (symbol "(") (symbol ")")

rws = ["module", "case", "let"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '?')
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

juxta :: Parser Expr
juxta = foldl1 App <$> some term

term :: Parser Expr
term =
  lexeme
    ( try parseFloat
        <|> try (parens parseInternalFunction)
        <|> try parseInteger
        <|> parseMaybe'
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
  pairs <- pair `sepBy` many (spaceChar <|> char ',')
  return (PDict (sig [AnyType] AnyType) pairs)
  where
    pair = do
      key <- try (dictKey <* string ":") <|> (variable <* string "=>")
      hspace
      val <- expr
      return (key, val)

dict :: Parser Expr
dict = do
  char '{'
  hspace
  x <- try $ dictContents
  char '}'
  return x

dictUpdate :: Parser Expr
dictUpdate = do
  char '{'
  hspace
  dct <- variable <|> dict
  hspace
  char '|'
  hspace
  optional $ char '{'
  hspace
  updates <- try dictContents <|> variable
  char '}'
  optional $ many (spaceChar <|> (char '}'))
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
      first <- lowerChar
      rest <- option [] identifier
      char '.'
      x <- dictKey
      return (DictAccess x (Atom (sig [DictionaryType] (DictionaryType)) (first : rest)))

tupleContents :: Parser [Expr]
tupleContents = (juxta <|> formula) `sepBy2` many (spaceChar <|> char ',')

sepBy2 p sep = liftM2 (:) p (some (sep >> p))

listContents :: Parser [Expr]
listContents = (juxta <|> formula) `sepBy` many (spaceChar <|> char ',')

range :: Parser Expr
range = do
  char '['
  hspace
  lBound <- term
  string ".."
  hspace
  uBound <- term
  char ']'
  return (PRange (sig [AnyType] AnyType) lBound uBound)

tuple :: Parser Expr
tuple = do
  char '('
  hspace
  x <- try tupleContents
  char ')'
  return (PTuple (sig [ListType AnyType] (ListType AnyType)) x)

list :: Parser Expr
list = do
  char '['
  hspace
  x <- try listContents
  char ']'
  return (PList (sig [ListType AnyType] (ListType AnyType)) x)

parseInternalFunction :: Parser Expr
parseInternalFunction = do
  string "InternalFunction"
  hspace
  f <- identifier
  args <- list <|> variable
  return (InternalFunction f args)

parseModule :: Parser Expr
parseModule = do
  string "module"
  hspace
  first <- upperChar
  rest <- many (letterChar <|> digitChar)
  optional hspace
  string "{"
  contents <- manyExpressions
  string "}"
  return (Module (first : rest) contents)

letin :: Parser Expr
letin = do
  string "let"
  hspace
  x <- variable
  hspace
  string "="
  hspace
  e1 <- term
  hspace
  string "in"
  e2 <- expr
  return (App (Lambda (sig [AnyType] AnyType) [x] e2) e1)

moduleAccess :: Parser Expr
moduleAccess = do
  first <- upperChar
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
  hspace
  string "::"
  hspace
  bindings <- identifier `sepBy1` (string "->" <* (many spaceChar))
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
  terms <- term `sepBy1` hspace
  string "="
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
  identifiers <- (variable <|> tuple) `sepBy` hspace
  string ":"
  body <- expr
  return (Lambda (sig [AnyType] AnyType) identifiers body)

ifthen :: Parser Expr
ifthen = do
  string "if"
  hspace
  cond <- term
  hspace
  string "then"
  hspace
  tr <- term
  hspace
  string "else"
  hspace
  PIf cond tr <$> term

ternary :: Parser Expr
ternary = do
  cond <- term
  string "?"
  hspace
  tr <- term
  string ":"
  hspace
  PIf cond tr <$> term

parseMaybe' :: Parser Expr
parseMaybe' = nothing <|> just
  where
    nothing = do
      string "Nothing"
      return PNothing
    just = do
      string "Just"
      hspace
      justVal <- expr
      return $ PJust (sig [AnyType] AnyType) justVal

parseFloat :: Parser Expr
parseFloat = do
  whole <- some digitChar
  char '.'
  decimal <- some digitChar
  return $ PFloat (read (whole ++ "." ++ decimal))

parseInteger :: Parser Expr
parseInteger = do
  whole <- some digitChar
  return $ PInteger $ read whole

noop :: Parser Expr
noop = do
  hspace *> lexeme eol
  return PNoop

comment :: Parser Expr
comment = do
  string "//"
  many $ anySingleBut '\n'
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
manyExpressions = (parseModule <|> expr) `sepBy` many (newline <|> char ';')

parseExprs' :: String -> Either (ParseErrorBundle String Void) [Expr]
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
