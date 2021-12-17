{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Types

spaceC :: Parser (Maybe Expr)
spaceC = space *> optional comment <* space

doesntLineBreak :: String
doesntLineBreak = ['!', ',', '+', '-', '}', '{', '[', '/', '(', '|', '=', ':', '?']

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
           <|> let'
           <|> try cons
           <|> case'
           <|> trait
           <|> dataDefinition
           <|> implementation
           <|> try ternary
           <|> try (binding Nothing Nothing)
           <|> try (typeDef [])
           <|> formula
           <|> block
       )
    <?> "expr"

formula :: Parser Expr
formula = makeExprParser juxta table <?> "formula"
  where
    table =
      [ [prefix "-" neg, not'],
        [pow],
        [mulOp, divOp, modOp],
        [addOrConcatOp, subOp],
        [gteOp, lteOp, gtOp, ltOp],
        [eqOp, notEqOp],
        [andOp],
        [orOp],
        [abs, sqrt, toFloat, toInteger, floor, round, ceiling],
        [pipeOp, mapPipe]
      ]
    prefix name fun = Prefix (do string name; return fun)
    neg n = case n of
      PInteger x -> PInteger $ negate x
      PFloat x -> PFloat $ negate x
    not' = Prefix (try $ spaceC *> string "!" <* spaceC >> return (Unaryop Not))
    toInteger = Prefix (try $ hspace *> string "toInteger" <* some spaceChar >> return (Unaryop ToInteger))
    abs = Prefix (try $ hspace *> string "abs" <* some spaceChar >> return (Unaryop Abs))
    toFloat = Prefix (try $ hspace *> string "toFloat" <* some spaceChar >> return (Unaryop ToFloat))
    floor = Prefix (try $ hspace *> string "floor" <* some spaceChar >> return (Unaryop Floor))
    round = Prefix (try $ hspace *> string "round" <* some spaceChar >> return (Unaryop Round))
    ceiling = Prefix (try $ hspace *> string "ceiling" <* some spaceChar >> return (Unaryop Ceiling))
    sqrt = Prefix (try $ hspace *> string "sqrt" <* some spaceChar >> return (Unaryop Sqrt))
    pow = InfixL (try $ hspace *> string "^" <* spaceC >> return (Binop Pow))
    notEqOp = InfixL (try $ spaceC *> string "!=" <* spaceC >> return (Binop NotEql))
    eqOp = InfixR (try $ spaceC *> string "==" <* spaceC >> return (Binop Eql))
    subOp = InfixL (try $ spaceC *> string "-" <* spaceC >> return (Binop Sub))
    addOrConcatOp = InfixL (try $ (spaceC >> string "+") <* notFollowedBy (char '+') <* spaceC >> return (Binop AddOrConcat))
    divOp = InfixL (try $ spaceC *> string "/" <* notFollowedBy (char '/') <* spaceC >> return (Binop Div))
    modOp = InfixL (try $ spaceC *> string "%" <* spaceC >> return (Binop Mod))
    mulOp = InfixL (try $ spaceC *> string "*" <* spaceC >> return (Binop Mul))
    andOp = InfixL (try $ spaceC *> string "&&" <* spaceC >> return (Binop And))
    gtOp = InfixL (try $ spaceC *> string ">" <* spaceC >> return (Cmp ">"))
    ltOp = InfixL (try $ spaceC *> string "<" <* spaceC >> return (Cmp "<"))
    gteOp = InfixL (try $ spaceC *> string ">=" <* spaceC >> return (Cmp ">="))
    lteOp = InfixL (try $ spaceC *> string "<=" <* spaceC >> return (Cmp "<="))
    orOp = InfixL (try $ spaceC *> string "||" <* notFollowedBy (char '>') <* spaceC >> return (Binop Or))
    mapPipe = InfixL (try $ spaceC *> string "||>" <* spaceC >> return (Binop MapPipe))
    pipeOp = InfixL (try $ spaceC *> string "|>" <* spaceC >> return pipe)

lexeme = L.lexeme hspace

symbol = L.symbol hspace

parens = between (symbol "(") (symbol ")")

brackets = between (symbol "[") (symbol "]")

rws :: [String]
rws = ["module", "case", "let", "else", "then"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> (letterChar <|> char '_' <|> char '@') <*> many (alphaNumChar <|> char '?' <|> char '!' <|> char '\'')
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
        <|> try (parens internalFunction)
        <|> try (parens lambda)
        <|> try parseInteger
        <|> try dataConstructor
        <|> moduleAccess
        <|> dictAccess
        <|> try dict
        <|> try dictUpdate
        <|> try range
        <|> true
        <|> false
        <|> try interpolatedString
        <|> parseString
        <|> escapedString -- HACK? to allow escaped strings inside interpolated strings
        <|> variable
        <|> try tuple
        <|> list
        <|> parens expr
    )
    <?> "term"

true :: Parser Expr
true = try $ string "true" >> return (PBool True)

false :: Parser Expr
false = try $ string "false" >> return (PBool False)

dictContents :: Parser Expr
dictContents = do
  pairs <- pair `endBy` many (spaceChar <|> char ',')
  return (PDict (sig [AnyType] AnyType) pairs)
  where
    pair = do
      key <- try (dictKey <* string ":") <|> variable <* string "=>"
      val <- expr
      return (key, val)

cons :: Parser Expr
cons = lexeme $ do
  consContents <- juxta `sepBy2` (hspace *> string "|" <* notFollowedBy (char '>' <|> char '|') <* hspace)
  return $ Cons consContents

dict :: Parser Expr
dict = do
  string "%{" <* spaceC
  x <- try dictContents
  spaceC *> char '}'
  return x

dictUpdate :: Parser Expr
dictUpdate = do
  string "%{" <* spaceC
  dct <- variable <|> dict
  spaceC *> char '|' <* spaceC
  optional $ string "%{" <* spaceC
  updates <- try dictContents <|> variable
  spaceC *> char '}'
  hspace *> optional (char '}')
  return (PDictUpdate dct updates)

dictAccess :: Parser Expr
dictAccess = dotKey <|> try dictDotKey
  where
    dotKey = do
      string "."
      dictKey <- identifier
      pure
        ( Lambda
            emptyLambdaEnv
            (sig [AnyType] AnyType)
            [Atom anyTypeSig "dictInPDictKeyLookup"]
            (DictAccess (PDictKey dictKey) (Atom anyTypeSig "dictInPDictKeyLookup"))
        )
    dictDotKey = do
      first <- lowerChar
      rest <- many (letterChar <|> digitChar)
      char '.'
      key <- dictKey
      return (DictAccess key (Atom (sig [DictType] DictType) (first : rest)))

tupleContents :: Parser [Expr]
tupleContents = juxtaFormula `sepBy2` many (spaceChar <|> char ',')

sepBy2 :: MonadPlus m => m a -> m sep -> m [a]
sepBy2 p sep = liftM2 (:) p (some (sep >> p))

listContents :: Parser [Expr]
listContents = juxtaFormula `endBy` many (spaceChar <|> char ',')

juxtaFormula :: Parser Expr
juxtaFormula = formula <|> juxta

range :: Parser Expr
range = do
  char '[' <* hspace
  lBound <- term -- FIXME: we want juxtaFormula here too
  hspace *> string ".." <* hspace
  uBound <- juxtaFormula
  char ']'
  return (PRange (sig [AnyType] AnyType) lBound uBound)

tuple :: Parser Expr
tuple = do
  char '(' <* space
  x <- try tupleContents
  spaceC *> char ')'
  return (PTuple (sig [TupleType AnyType] (TupleType AnyType)) x)

list :: Parser Expr
list = do
  char '[' <* spaceC
  x <- try listContents
  spaceC *> char ']'
  return (PList (sig [ListType AnyType] (ListType AnyType)) x)

internalFunction :: Parser Expr
internalFunction = do
  string "HFI" <* space
  f <- identifier
  args <- list <|> variable
  return (HFI f args)

module' :: Parser Expr
module' = do
  string "module" <* hspace
  first <- upperChar
  rest <- many (letterChar <|> digitChar)
  hspace *> string "{" <* spaceC
  contents <- manyExpressions
  spaceC *> string "}"
  return (Module (first : rest) contents)

case' :: Parser Expr
case' = do
  string "case" <* space
  predicate <- expr
  hspace *> string "{" <* spaceC
  cases <- singleCase `endBy1` (expressionSep <* spaceC)
  spaceC *> string "}"
  return $ PCase (sig [AnyType] AnyType) predicate cases
  where
    singleCase = do
      casePred <- try (term <* string ":") <* spaceC
      caseDo <- expr
      return (casePred, caseDo)

let' :: Parser Expr
let' = do
  string "let" <* spaceC
  pairs <- binding Nothing Nothing `sepBy1` many (spaceChar <|> char ',')
  spaceC *> string ":" <* spaceC
  body <- expr
  return $ foldr foldFun body pairs
  where
    foldFun kv acc = case kv of
      Binop Assign name body -> App (Lambda emptyLambdaEnv (sig [AnyType] AnyType) [name] acc) body

moduleAccess :: Parser Expr
moduleAccess = do
  first <- upperChar
  rest <- many alphaNumChar
  hspace
  args <- many (char '.' <|> alphaNumChar)
  return (Atom (sig [AnyType] AnyType) ([first] ++ rest ++ args))

variable :: Parser Expr
variable = Atom (sig [] AnyType) `fmap` identifier

dictKey :: Parser Expr
dictKey = PDictKey <$> identifier

typeDef :: [(String, String)] -> Parser Expr
typeDef typeConstructors = do
  name <- try $ identifier <* space
  args <- try $ option [] $ typeBindings typeConstructors
  space *> string "=>" <* spaceC
  rtrn <- typeBinding typeConstructors
  return $ PTypeSig TypeSig {typeSigName = Just name, typeSigModule = Nothing, typeSigTraitBinding = Nothing, typeSigImplementationBinding = Nothing, typeSigIn = args, typeSigReturn = rtrn}

typeBindings :: [(String, String)] -> Parser [LangType]
typeBindings typeConstructors = typeBinding typeConstructors `sepBy1` ((string ":" <|> string ",") <* hspace)

typeBinding :: [(String, String)] -> Parser LangType
typeBinding typeConstructors = funcType <|> try variableTypeConstructor <|> typeVariable <|> concreteTypeConstructor <|> listType
  where
    funcType = do
      string "(" <* space
      bindings <- typeBindings typeConstructors
      space *> string ")"
      let (args, rtrn) = (init bindings, last bindings)
      return $ FunctionType args rtrn
    typeVariable = do
      first <- lowerChar
      rest <- many (letterChar <|> digitChar)
      pure $ toLangType (first : rest)
    variableTypeConstructor = do
      typeConstructor <- choice (map (string . fst) typeConstructors) <* hspace
      args <- typeBinding typeConstructors
      pure $ TraitVariableType (fromJust $ List.lookup typeConstructor typeConstructors) args
    concreteTypeConstructor = do
      first <- upperChar
      rest <- many (letterChar <|> digitChar) <* hspace
      arg <- option AnyType typeVariable
      let dataConstructor = first : rest
      let existingType = toLangType dataConstructor
      if existingType /= AnyType
        then pure existingType
        else pure $ TypeConstructorType (first : rest) arg
    listType = do
      string "[" <* space
      bindings <- typeBindings typeConstructors
      space *> string "]"
      let (tp : _) = bindings
      return $ ListType tp

binding :: Maybe TypeConstructor -> Maybe String -> Parser Expr
binding traitBinding implementationBinding = do
  name <- term
  args <- (term <|> list) `sepBy` hspace
  string "=" <* space
  body <- expr
  let Atom _ nameStr = name
  let funSig = TypeSig {typeSigName = Just nameStr, typeSigModule = Nothing, typeSigImplementationBinding = implementationBinding, typeSigTraitBinding = traitBinding, typeSigIn = replicate (length args) AnyType, typeSigReturn = AnyType}
  if null args
    then return $ Binop Assign name body
    else case name of
      (Atom _ _) -> return $ Binop Assign name (Lambda emptyLambdaEnv funSig args body)
      destructureObject -> return $ Binop Assign destructureObject body

pipe :: Expr -> Expr -> Expr
pipe = Binop Pipe

lambda :: Parser Expr
lambda = do
  identifiers <- (try (parens cons) <|> variable <|> list <|> tuple) `sepBy` hspace
  string ":" <* notFollowedBy (string ":") -- NOTE: can remove since :: is not a thing?
  Lambda emptyLambdaEnv (sig (replicate (length identifiers) AnyType) AnyType) identifiers <$> expr

import' :: Parser Expr
import' = do
  string "import" <* hspace
  PImport <$> interpolatedString

ifelse :: Parser Expr
ifelse = do
  string "if" <* hspace
  cond <- juxtaFormula
  spaceC *> string "then" <* spaceC
  tr <- lexeme block <|> juxtaFormula
  string "else" <* spaceC
  tr2 <- lexeme block <|> juxtaFormula
  return $ PCase (sig [AnyType] AnyType) cond [(PBool True, tr), (PBool False, tr2)]

ternary :: Parser Expr
ternary = do
  cond <- juxtaFormula <* string "?" <* spaceC
  tr <- lexeme block <|> juxtaFormula
  string ":" <* spaceC
  tr2 <- lexeme block <|> juxtaFormula
  return $ PCase (sig [AnyType] AnyType) cond [(PBool True, tr), (PBool False, tr2)]

dataDefinition :: Parser Expr
dataDefinition = do
  string "data" <* spaceC
  typeCons <- identifier
  space *> string "=" <* spaceC
  constructors <- constructor `sepBy1` char '|'
  return $ PDataDefinition typeCons constructors
  where
    constructor = do
      valueCons <- space *> identifier <* hspace
      valueArgs <- many identifier
      return (valueCons, valueArgs)

trait :: Parser Expr
trait = do
  string "trait" <* hspace
  name <- identifier <* hspace
  vars <- many identifier
  let typeConstructors = [(head vars, name) | not (null vars)]
  hspace *> string "{" <* spaceC
  bindings <- (try (binding (Just name) Nothing) <|> typeDef typeConstructors) `endBy1` (expressionSep <* spaceC)
  space *> string "}"
  let (types, funs) =
        List.partition
          ( \case
              PTypeSig _ -> True
              _ -> False
          )
          bindings
  return $ PTrait name types funs

block :: Parser Expr
block = do
  hspace *> string "{" <* spaceC
  exprs <- (expr <|> noop) `endBy1` (expressionSep <* spaceC)
  spaceC *> string "}"
  return $ Block exprs

implementation :: Parser Expr
implementation = do
  string "implement" <* hspace
  trait <- identifier
  hspace *> string "for" <* hspace
  typeConstructor <- identifier
  hspace *> string "{" <* spaceC
  bindings <- try (binding (Just trait) (Just typeConstructor)) `endBy` (expressionSep <* spaceC)
  spaceC *> string "}"
  return $ PImplementation trait typeConstructor bindings

dataConstructor :: Parser Expr
dataConstructor = do
  first <- upperChar
  rest <- many (letterChar <|> digitChar) <* notFollowedBy (char '.') <* hspace
  args <- many term
  return (PDataConstructor (first : rest) args)

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
manyExpressions = (lexeme module' <|> expr <|> noop) `endBy1` expressionSep

expressionSep :: Parser String
expressionSep = many (newlineWithoutAdjacentInfixOp <|> char ';')
  where
    newlineWithoutAdjacentInfixOp = try $ lookAhead (noneOf doesntLineBreak) *> hspace *> newline <* notFollowedBy (spaceC *> oneOf doesntLineBreak)

parseExprs' :: String -> String -> Either (ParseErrorBundle String Void) [Expr]
parseExprs' = parse (manyExpressions <* eof)

interpolatedString :: Parser Expr
interpolatedString =
  PInterpolatedString <$> between (char '"') (char '"') (some $ between (string "#{") (string "}") expr <|> parseStringContent)

escapedString :: Parser Expr
escapedString =
  PString . T.pack <$> between (string "\\\"") (string "\\\"") (many escapedLangChar)
  where
    escapedLangChar = alphaNumChar

parseString :: Parser Expr
parseString =
  PString . T.pack <$> between (char '"') (char '"') (many langChar)

parseStringContent :: Parser Expr
parseStringContent = do
  s <- langChar
  return $ PString $ T.pack [s]

langChar :: Parser Char
langChar = unescaped <|> escaped
  where
    unescaped = noneOf ['\\', '"']
    escaped = char '\\' *> escapedChar

escapedChar :: Parser Char
escapedChar = choice $ map ch alist
  where
    ch (x, y) = y <$ char x
    alist =
      [ ('b', '\b'),
        ('f', '\f'),
        ('n', '\n'),
        ('r', '\r'),
        ('t', '\t'),
        ('\\', '\\'),
        ('\"', '\"')
      ]

sig :: [LangType] -> LangType -> TypeSig
sig inn out = TypeSig {typeSigName = Nothing, typeSigModule = Nothing, typeSigTraitBinding = Nothing, typeSigImplementationBinding = Nothing, typeSigIn = inn, typeSigReturn = out}
