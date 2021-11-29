{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.List qualified as List
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Types

doesntLineBreak :: String
doesntLineBreak = ['!', ',', '+', '-', '{', '[', '/', '(', '|', '=', ':', '?']

-- doesntLineBreak :: [String]
-- oesntLineBreak = [",", "+", "-", "{", "[", "(", "|", "=", ":", "?"]

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

sc :: Parser ()
sc = L.space hspace1 lineComment blockComment

expr :: Parser Expr
expr =
  shebang <|> try comment <|> sc
    *> ( import'
           <|> ifelse
           <|> try (typeDef [])
           <|> letBinding
           <|> parseCase
           <|> traitDefinition
           <|> dataDefinition
           <|> implementationDefinition
           <|> try lambda
           <|> try ternary
           <|> try (function Nothing)
           <|> formula
           <|> noop
       )
    <?> "expr"

formula :: Parser Expr
formula = makeExprParser juxta table <?> "formula"
  where
    table =
      [ [prefix "-" neg, not'],
        [pow],
        [mulOp, divOp, modOp],
        [addOp, subOp],
        [gteOp, lteOp, gtOp, ltOp],
        [eqOp, notEqOp],
        [andOp],
        [orOp],
        [concatOp],
        [abs, sqrt, toFloat, toInteger, floor, round, ceiling],
        [consOp],
        [pipeOp]
      ]
    prefix name fun = Prefix (do string name; return fun)
    neg n = case n of
      PInteger x -> PInteger $ negate x
      PFloat x -> PFloat $ negate x
    not' = Prefix (try $ space *> string "!" <* space >> return (Unaryop Not))
    toInteger = Prefix (try $ hspace *> string "toInteger" <* some spaceChar >> return (Unaryop ToInteger))
    abs = Prefix (try $ hspace *> string "abs" <* some spaceChar >> return (Unaryop Abs))
    toFloat = Prefix (try $ hspace *> string "toFloat" <* some spaceChar >> return (Unaryop ToFloat))
    floor = Prefix (try $ hspace *> string "floor" <* some spaceChar >> return (Unaryop Floor))
    round = Prefix (try $ hspace *> string "round" <* some spaceChar >> return (Unaryop Round))
    ceiling = Prefix (try $ hspace *> string "ceiling" <* some spaceChar >> return (Unaryop Ceiling))
    sqrt = Prefix (try $ hspace *> string "sqrt" <* some spaceChar >> return (Unaryop Sqrt))
    pow = InfixL (try $ hspace *> string "^" <* space >> return (Binop Pow))
    notEqOp = InfixL (try $ space *> string "!=" <* space >> return (Binop NotEql))
    eqOp = InfixR (try $ space *> string "==" <* space >> return (Binop Eql))
    subOp = InfixL (try $ space *> string "-" <* space >> return (Binop Sub))
    addOp = InfixL (try $ (space >> string "+") <* notFollowedBy (char '+') <* space >> return (Binop Add))
    divOp = InfixL (try $ space *> string "/" <* notFollowedBy (char '/') <* space >> return (Binop Div))
    modOp = InfixL (try $ space *> string "%" <* space >> return (Binop Mod))
    mulOp = InfixL (try $ space *> string "*" <* space >> return (Binop Mul))
    andOp = InfixL (try $ space *> string "&&" <* space >> return (Binop And))
    gtOp = InfixL (try $ space *> string ">" <* space >> return (Cmp ">"))
    ltOp = InfixL (try $ space *> string "<" <* space >> return (Cmp "<"))
    gteOp = InfixL (try $ space *> string ">=" <* space >> return (Cmp ">="))
    lteOp = InfixL (try $ space *> string "<=" <* space >> return (Cmp "<="))
    orOp = InfixL (try $ space *> string "||" <* space >> return (Binop Or))
    concatOp = InfixL (try $ space *> string "++" <* space >> return (Binop Concat))
    consOp = InfixL (try $ space *> string "::" <* space >> return (Binop Cons))
    pipeOp = InfixL (try $ space *> string "|>" <* space >> return (Binop Pipe))

lexeme = L.lexeme hspace

symbol = L.symbol hspace

parens = between (symbol "(") (symbol ")")

brackets = between (symbol "[") (symbol "]")

rws :: [String]
rws = ["module", "case", "let"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> (letterChar <|> char '_' <|> char '@') <*> many (alphaNumChar <|> char '?' <|> char '\'')
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
        <|> try parseInterpolatedString
        <|> parseString
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
  listContents <- identifier `sepBy2` (space *> string "::" <* space)
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
  space *> char '}'
  hspace *> optional (char '}')
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
  space *> char ')'
  return (PTuple (sig [ListType AnyType] (ListType AnyType)) x)

list :: Parser Expr
list = do
  char '[' <* space
  x <- try listContents
  space *> char ']'
  return (PList (sig [ListType AnyType] (ListType AnyType)) x)

parseInternalFunction :: Parser Expr
parseInternalFunction = do
  string "HFI" <* space
  f <- identifier
  args <- list <|> variable
  return (HFI f args)

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
  space *> string "|" <* space
  cases <- singleCase `sepBy1` (space *> char '|' <* hspace)
  return $ PCase (sig [AnyType] AnyType) predicate cases
  where
    singleCase = do
      casePred <- try (term <* string ":") <* space
      caseDo <- term <|> formula
      return (casePred, caseDo)

letBinding :: Parser Expr
letBinding = do
  string "let" <* space
  pairs <- pair `sepBy1` many (spaceChar <|> char ',')
  space *> string ":" <* space
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
  return $ PTypeSig TypeSig {typeSigName = Just name, typeSigTraitBinding = Nothing, typeSigIn = args, typeSigReturn = rtrn}

typeBinding :: [(String, String)] -> Parser [LangType]
typeBinding typeConstructors = (funcType <|> try variableTypeConstructor <|> typeVariable <|> concreteTypeConstructor <|> listType) `sepBy1` ((string ":" <|> string ",") <* hspace)
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
      args <- typeBinding typeConstructors
      let typeConstructor = first : rest
      case List.find (\x -> typeConstructor == fst x) typeConstructors of
        Just (_, tCons) -> pure $ TypeConstructorType tCons $ head args
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

function :: Maybe TypeConstructor -> Parser Expr
function traitBinding = do
  terms <- term `sepBy1` hspace
  string "=" <* space
  let (name : args) = terms
  body <- expr
  let Atom _ nameStr = name
  let funSig = TypeSig {typeSigName = Just nameStr, typeSigTraitBinding = traitBinding, typeSigIn = [], typeSigReturn = AnyType}
  if null args
    then return $ Binop Assign name body
    else case name of
      (Atom _ _) -> return $ Binop Assign name (Lambda funSig args body)
      destructureObject -> return $ Binop Assign destructureObject body

lambda :: Parser Expr
lambda = do
  identifiers <- (variable <|> tuple) `sepBy` hspace
  string ":" <* notFollowedBy (string ":")
  Lambda (sig [AnyType] AnyType) identifiers <$> expr

import' :: Parser Expr
import' = do
  string "import" <* space
  filePath <- parseInterpolatedString
  return $ PImport filePath

ifelse :: Parser Expr
ifelse = do
  string "if" <* space
  cond <- term
  space *> string ":" <* space
  tr <- term
  space *> string "else" <* space
  tr2 <- term
  return $ PCase (sig [AnyType] AnyType) cond [(PBool True, tr), (PBool False, tr2)]

dataDefinition :: Parser Expr
dataDefinition = do
  string "data" <* space
  typeCons <- identifier
  space *> string "=" <* space
  constructors <- constructor `sepBy1` char '|'
  return $ PDataDefinition typeCons constructors
  where
    constructor = do
      valueCons <- space *> identifier <* hspace
      valueArgs <- many identifier
      return (valueCons, valueArgs)

traitDefinition :: Parser Expr
traitDefinition = do
  string "trait" <* space
  name <- identifier <* space
  vars <- many identifier
  let typeConstructors = [(head vars, name) | not (null vars)]
  space *> string ":" <* space
  space *> string "|" <* hspace
  bindings <- (try (typeDef typeConstructors) <|> function (Just name)) `sepBy1` (space *> char '|' <* hspace)
  let (types, funs) =
        List.partition
          ( \case
              PTypeSig _ -> True
              _ -> False
          )
          bindings
  return $ PTrait name types funs

implementationDefinition :: Parser Expr
implementationDefinition = do
  string "implement" <* space
  trait <- identifier
  space *> string "for" <* space
  typeConstructor <- identifier
  space *> string ":" <* space
  space *> string "|" <* hspace
  functions <- function (Just typeConstructor) `sepBy1` many (space *> char '|' <* hspace)
  return $ PImplementation trait typeConstructor functions

dataConstructor :: Parser Expr
dataConstructor = do
  first <- upperChar
  rest <- many (letterChar <|> digitChar) <* hspace
  args <- many term
  return (PDataConstructor (first : rest) args)

ternary :: Parser Expr
ternary = do
  cond <- term <* string "?" <* space
  tr <- term
  string ":" <* hspace
  tr2 <- term
  return $ PCase (sig [AnyType] AnyType) cond [(PBool True, tr), (PBool False, tr2)]

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
  hspace *> eol
  return PNoop

comment :: Parser Expr
comment = do
  hspace *> string "//"
  many $ anySingleBut '\n'
  return PNoop

shebang :: Parser Expr
shebang = do
  string "#!"
  many $ anySingleBut '\n'
  return PNoop

parseExpr :: String -> Expr
parseExpr e = head $ parseExprs e

parseExprs :: String -> [Expr]
parseExprs s = case parseExprs' "unknown" s of
  Left err -> error $ show err
  Right exprs -> exprs

manyExpressions :: Parser [Expr]
manyExpressions = (parseModule <|> expr <|> noop) `endBy` many (newlineWithoutAdjacentInfixOp <|> char ';')
  where
    newlineWithoutAdjacentInfixOp = try $ lookAhead (noneOf doesntLineBreak) *> hspace *> newline <* notFollowedBy (space *> oneOf doesntLineBreak)

parseExprs' :: String -> String -> Either (ParseErrorBundle String Void) [Expr]
parseExprs' source = parse (manyExpressions <* eof) source

parseInterpolatedString :: Parser Expr
parseInterpolatedString = do
  string "\""
  parts <- some $ between (symbol "#{") (symbol "}") (formula <|> term) <|> parseStringContent
  string "\""
  return $ PInterpolatedString parts

parseString :: Parser Expr
parseString = do
  string "\""
  s <- many $ escapedChars <|> noneOf ['\\', '"']
  string "\""
  return $ PString s

parseStringContent :: Parser Expr
parseStringContent = do
  s <- escapedChars <|> noneOf ['\\', '"']
  return $ PString [s]

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  oneOf ['\\', '"']

sig :: [LangType] -> LangType -> TypeSig
sig inn out = TypeSig {typeSigName = Nothing, typeSigTraitBinding = Nothing, typeSigIn = inn, typeSigReturn = out}
