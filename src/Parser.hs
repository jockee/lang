{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Functor ((<&>))
import Data.List qualified as List (find)
import Data.Void
import Debug.Trace
import Eval ()
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

doesntLineBreak :: String
doesntLineBreak = ['!', ',', '+', '-', '{', '[', '(', '|', '=', ':', '?']

-- doesntLineBreak :: [String]
-- doesntLineBreak = [",", "+", "-", "{", "[", "(", "|", "=", ":", "?"]

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
           <|> try (typeDef [])
           <|> letBinding
           <|> parseCase
           <|> try lambda
           <|> parseDataDefinition
           <|> traitDefinition
           <|> implementationDefinition
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
        [pipeOp],
        [consOp]
      ]
    prefix name fun = Prefix (do string name; return fun)
    neg n = case n of
      PInteger x -> PInteger $ negate x
      PFloat x -> PFloat $ negate x
    not' (PBool b) = PBool $ not b
    notEqOp = InfixL (try $ space *> string "!=" <* optional space >> return (Binop NotEql))
    eqOp = InfixR (try $ space *> string "==" <* optional space >> return (Binop Eql))
    subOp = InfixL (try $ space *> string "-" <* optional space >> return (Binop Sub))
    addOp = InfixL (try $ (space >> string "+") <* notFollowedBy (char '+') <* optional space >> return (Binop Add))
    mulOp = InfixL (try $ space *> string "*" <* optional space >> return (Binop Mul))
    andOp = InfixL (try $ space *> string "&&" <* optional space >> return (Binop And))
    gtOp = InfixL (try $ space *> string ">" <* optional space >> return (Cmp ">"))
    ltOp = InfixL (try $ space *> string "<" <* optional space >> return (Cmp "<"))
    gteOp = InfixL (try $ space *> string ">=" <* optional space >> return (Cmp ">="))
    lteOp = InfixL (try $ space *> string "<=" <* optional space >> return (Cmp "<="))
    orOp = InfixL (try $ space *> string "||" <* optional space >> return (Binop Or))
    concatOp = InfixL (try $ space *> string "++" <* optional space >> return (Binop Concat))
    consOp = InfixL (try $ space *> string "::" <* optional space >> return (Binop Cons))
    pipeOp = InfixL (try $ space *> string "|>" <* optional space >> return (Binop Pipe))

lexeme = L.lexeme hspace

symbol = L.symbol hspace

parens = between (symbol "(") (symbol ")")

brackets = between (symbol "[") (symbol "]")

rws :: [String]
rws = ["module", "case", "let"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '?' <|> char '\'')
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
        <|> try dataConstructor
        <|> moduleAccess
        <|> dictAccess
        <|> try dict
        <|> try dictUpdate
        <|> try range
        <|> list
        <|> true
        <|> false
        <|> parseInterpolatedString
        <|> try consList
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
      key <- try (dictKey <* string ":") <|> variable <* string "=>"
      val <- expr
      return (key, val)

consList :: Parser Expr
consList = do
  char '(' <* space
  listContents <- identifier `sepBy` (space *> string "::" <* space)
  space *> char ')'
  return $ ConsList listContents

dict :: Parser Expr
dict = do
  char '{' <* space
  x <- try dictContents
  space *> char '}'
  return x

dictUpdate :: Parser Expr
dictUpdate = do
  char '{' <* space
  dct <- variable <|> dict
  space *> char '|' <* space
  optional $ char '{' <* space
  updates <- try dictContents <|> variable
  char '}'
  optional $ many (spaceChar <|> char '}')
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
      return (DictAccess x (Atom (sig [DictionaryType] DictionaryType) (first : rest)))

tupleContents :: Parser [Expr]
tupleContents = juxta `sepBy2` many (spaceChar <|> char ',')

sepBy2 :: MonadPlus m => m a -> m sep -> m [a]
sepBy2 p sep = liftM2 (:) p (some (sep >> p))

listContents :: Parser [Expr]
listContents = (juxta <|> formula) `sepBy` many (spaceChar <|> char ',')

range :: Parser Expr
range = do
  char '(' <* space
  lBound <- term
  string ".." <* space
  uBound <- term
  char ')'
  return (PRange (sig [AnyType] AnyType) lBound uBound)

tuple :: Parser Expr
tuple = do
  char '(' <* space
  x <- try tupleContents
  char ')'
  return (PTuple (sig [ListType AnyType] (ListType AnyType)) x)

list :: Parser Expr
list = do
  char '[' <* space
  x <- try listContents
  char ']'
  return (PList (sig [ListType AnyType] (ListType AnyType)) x)

parseInternalFunction :: Parser Expr
parseInternalFunction = do
  string "InternalFunction" <* space
  f <- identifier
  args <- list <|> variable
  return (InternalFunction f args)

parseModule :: Parser Expr
parseModule = do
  string "module" <* space
  first <- upperChar
  rest <- many (letterChar <|> digitChar)
  space *> string "{"
  contents <- manyExpressions
  string "}"
  return (Module (first : rest) contents)

parseCase :: Parser Expr
parseCase = do
  string "case" <* space
  predicate <- expr
  space *> string ":" <* space
  string "|" <* space
  cases <- singleCase `sepBy1` many (spaceChar <|> char '|')
  return $ PCase (sig [AnyType] AnyType) predicate cases
  where
    singleCase = do
      casePred <- space *> try (term <* string ":") <* space
      caseDo <- term <|> formula
      return (casePred, caseDo)

letBinding :: Parser Expr
letBinding = do
  string "let" <* space
  pairs <- pair `sepBy1` many (spaceChar <|> char ',')
  space *> string ":"
  body <- expr
  return $ foldr foldFun body pairs
  where
    foldFun (pairKey, pairVal) acc =
      App (Lambda (sig [AnyType] AnyType) [pairKey] acc) pairVal
    pair = do
      key <- space *> try (term <* string "=") <* space
      val <- term <|> formula
      return (key, val)

moduleAccess :: Parser Expr
moduleAccess = do
  first <- upperChar
  rest <- option [] identifier
  char '.'
  x <- identifier
  return (Atom (sig [AnyType] AnyType) ([first] ++ rest ++ "." ++ x))

variable :: Parser Expr
variable = Atom (sig [] AnyType) `fmap` identifier

dictKey :: Parser Expr
dictKey = PDictKey `fmap` identifier

typeDef :: [(String, String)] -> Parser Expr
typeDef typeConstructors = do
  name <- identifier <* space
  string "#" *> space
  bindings <- typeBinding typeConstructors
  let (args, rtrn) = (init bindings, last bindings)
  return $ PTypeSig TypeSig {typeSigName = Just name, typeSigIn = args, typeSigReturn = rtrn}

typeBinding :: [(String, String)] -> Parser [LangType]
typeBinding typeConstructors = (funcType <|> try variableTypeConstructor <|> typeVariable <|> concreteTypeConstructor <|> listType) `sepBy1` ((string ":" <|> string ",") <* many spaceChar)
  where
    funcType = do
      string "(" <* space
      bindings <- typeBinding typeConstructors
      space *> string ")"
      let (args, rtrn) = (init bindings, last bindings)
      return $ FunctionType args rtrn
    typeVariable = do
      first <- lowerChar
      rest <- many (letterChar <|> digitChar)
      pure $ toLangType (first : rest)
    variableTypeConstructor = do
      first <- lowerChar
      rest <- many (letterChar <|> digitChar) <* space
      arg <- typeVariable
      let typeConstructor = first : rest
      case List.find (\x -> typeConstructor == fst x) typeConstructors of
        Just (_, tCons) -> pure $ TypeConstructorType tCons arg
        Nothing -> error ("Can't find type constructor variable binding for " ++ show typeConstructor)
    concreteTypeConstructor = do
      first <- upperChar
      rest <- many (letterChar <|> digitChar) <* space
      arg <- option AnyType typeVariable
      let dataConstructor = first : rest
      let existingType = toLangType dataConstructor
      if existingType /= AnyType
        then pure existingType
        else pure $ TypeConstructorType (first : rest) arg
    listType = do
      string "[" <* space
      bindings <- typeBinding typeConstructors
      space *> string "]"
      let (tp : _) = bindings
      return $ ListType tp

function :: Parser Expr
function = do
  terms <- term `sepBy1` hspace
  string "=" <* space
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
  identifiers <- (variable <|> tuple) `sepBy` space
  string ":" <* notFollowedBy (string ":")
  Lambda (sig [AnyType] AnyType) identifiers <$> expr

ifthen :: Parser Expr
ifthen = do
  string "if" <* space
  cond <- term
  space *> string "then" <* space
  tr <- term
  space *> string "else" <* space
  PIf cond tr <$> term

parseDataDefinition :: Parser Expr
parseDataDefinition = do
  string "data" <* space
  typeCons <- identifier
  space *> string "=" <* space
  constructors <- constructor `sepBy1` many (spaceChar <|> char '|')
  return $ PDataDefinition typeCons constructors
  where
    constructor = do
      valueCons <- space *> identifier <* space
      valueArgs <- identifier `sepBy` many spaceChar
      return (valueCons, valueArgs)

traitDefinition :: Parser Expr
traitDefinition = do
  string "trait" *> space
  name <- identifier <* space
  vars <- identifier `sepBy` many spaceChar
  let varMappings = [(head vars, name) | not (null vars)]
  space *> string ":" <* space
  space *> string "|" <* space
  defs <- typeDef varMappings `sepBy1` many (spaceChar <|> char '|')
  return $ PTrait name defs

implementationDefinition :: Parser Expr
implementationDefinition = do
  string "implement" <* space
  trait <- identifier
  space *> string "for" <* space
  dtype <- identifier
  space *> string ":" <* space
  string "|" *> space
  functions <- function `sepBy1` many (spaceChar <|> char '|')
  return $ PImplementation trait dtype functions

dataConstructor :: Parser Expr
dataConstructor = do
  first <- upperChar
  rest <- many (letterChar <|> digitChar) <* space
  args <- many term
  return (PDataConstructor (first : rest) args)

ternary :: Parser Expr
ternary = do
  cond <- term <* string "?" <* space
  tr <- term
  string ":" <* space
  PIf cond tr <$> term

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
manyExpressions = (parseModule <|> expr) `sepBy` many (splitAfterInfix <|> splitBeforeInfix <|> char ';')
  where
    splitAfterInfix = try $ lookAhead (noneOf doesntLineBreak) *> hspace *> newline
    splitBeforeInfix = newline <* notFollowedBy (space *> oneOf doesntLineBreak)

parseExprs' :: String -> Either (ParseErrorBundle String Void) [Expr]
parseExprs' = parse (allOf manyExpressions) "stdin"

parseInterpolatedString :: Parser Expr
parseInterpolatedString = do
  string "\""
  parts <- some $ between (symbol "#{") (symbol "}") (formula <|> term) <|> parseStringContent
  string "\""
  return $ PString parts

parseStringContent :: Parser Expr
parseStringContent = do
  s <- escapedChars <|> noneOf ['\\', '"']
  return $ PChar [s]

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  oneOf ['\\', '"']

sig :: [LangType] -> LangType -> TypeSig
sig inn out = TypeSig {typeSigName = Nothing, typeSigIn = inn, typeSigReturn = out}
