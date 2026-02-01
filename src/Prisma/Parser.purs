module Prisma.Parser where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (filter, find, fromFoldable, catMaybes)
import Data.Array as A
import Data.Either (Either(..), blush, hush)
import Data.List.Types (List)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)

------ StringParser imports
import StringParser (Parser, unParser, try, fail)
import StringParser as StringParser
import StringParser.CodePoints (char, satisfy, string, anyChar)
import StringParser.Combinators (sepBy1, many, lookAhead, option, sepBy)

--
import Data.String.CodePoints (codePointFromChar)
import Data.CodePoint.Unicode as U

import Prisma.AST

-- ==========================================
-- PARSER IMPLEMENTATION
-- ==========================================

-- --- 1. Low Level Helpers ---

skipMany :: forall a. Parser a -> Parser Unit
skipMany p = void $ many p

listToString :: List Char -> String
listToString cs = fromCharArray (fromFoldable cs)

isAlpha :: Char -> Boolean
isAlpha = U.isAlpha <<< codePointFromChar

isAlphaNum :: Char -> Boolean
isAlphaNum c = U.isAlphaNum (codePointFromChar c) || c == '_' || c == '-' || c == '.'

isSpace :: Char -> Boolean
isSpace = U.isSpace <<< codePointFromChar

isHorizontalSpace :: Char -> Boolean
isHorizontalSpace c = c == ' ' || c == '\t'

-- Handles whitespace but IGNORES //_ comments (so we can parse them)
whitespace :: Parser Unit
whitespace = skipMany (spaceOrComment)
  where
    spaceOrComment =
      (void $ satisfy isSpace) <|>
      parseNormalComment

    parseNormalComment = try do
      void $ string "//"
      -- Look ahead: if next char is _, this is a meta comment, don't consume!
      c <- lookAhead (option ' ' anyChar)
      if c == '_'
        then fail "Is Meta Comment"
        else void $ many (satisfy \x -> x /= '\n')

-- --- 2. Shared Token Parsers ---

ident :: Parser String
ident = do
  first <- satisfy isAlpha
  rest  <- many (satisfy isAlphaNum)
  pure $ listToString (pure first <> rest)

quotedString :: Parser String
quotedString = do
  void $ char '"'
  content <- many (satisfy \c -> c /= '"')
  void $ char '"'
  pure $ listToString content

envFunc :: Parser PrismaValue
envFunc = do
  void $ string "env"
  void $ whitespace *> char '(' *> whitespace
  varName <- quotedString
  void $ whitespace *> char ')'
  pure $ PVEnv varName

-- Parser for numeric literals (e.g., 100, -5, 3.14)
number :: Parser PrismaValue
number = do
  first <- satisfy \c -> (c >= '0' && c <= '9') || c == '-'
  rest  <- many (satisfy \c -> (c >= '0' && c <= '9') || c == '.')
  pure $ PVNumber (listToString (pure first <> rest))


-- Parses any Prisma Value
prismaValue :: Parser PrismaValue
prismaValue = fix \p ->
  let
    genericArray = do
      void $ string "[" *> whitespace
      vals <- sepBy p (try (whitespace *> string "," *> whitespace))
      void $ whitespace *> string "]"
      pure $ fromFoldable vals

    genericFunc = try do
      name <- ident
      void $ trySpaceSurrounding $ char '('
      args <- sepBy p $ trySpaceSurrounding $ char ','
      void $ many (satisfy isHorizontalSpace) *> char ')'
      pure $ PVFunc name (fromFoldable args)
  in
    try envFunc                   <|>
    try (do
       k <- ident
       void $ trySpaceSurrounding $ string ":"
       v <- p
       pure $ PVKeyVal k v
    )                             <|>
    genericFunc                   <|>
    number                        <|>
    (PVString <$> quotedString)   <|>
    (PVArray <$> genericArray)    <|>
    (PVIdent <$> ident)

trySpaceSurrounding :: forall a. Parser a -> Parser (List Char)
trySpaceSurrounding rule =
  try $ many (satisfy isHorizontalSpace) *> rule *> many (satisfy isHorizontalSpace)

-- --- 3. Attribute Parsers ---

parseAttributeArgs :: Parser PrismaValue
parseAttributeArgs = do
    void $ trySpaceSurrounding $ char '('
    args <- sepBy prismaValue $ trySpaceSurrounding $ char ','
    void $ many (satisfy isHorizontalSpace) *> char ')'

    let argsArr = fromFoldable args
    pure $ if (A.length argsArr) == 1 then
             (case argsArr of
                [x] -> x
                _ -> PVString "")
           else PVFunc "list" argsArr

parseAttribute :: Parser Property
parseAttribute = do
  void $ char '@'
  key <- ident
  val <- option (PVString "true") (try parseAttributeArgs)
  pure $ Property ("@" <> key) { val, meta: [] }

metaValue :: Parser PrismaValue
metaValue = fix \p ->
  (PVString <$> quotedString) <|>
  try (do
    name <- ident
    void $ many (satisfy isHorizontalSpace) *> char '(' *> many (satisfy isHorizontalSpace)
    args <- sepBy p $ trySpaceSurrounding $ char ','
    void $ many (satisfy isHorizontalSpace) *> char ')'
    pure $ PVFunc name (fromFoldable args)
  ) <|>
  (PVIdent <$> ident)

metaAttr :: Parser Property
metaAttr = do
  void $ char '@'
  key <- ident
  val <- option (PVString "true") (try do
    void $ many (satisfy isHorizontalSpace) *> char '(' *> many (satisfy isHorizontalSpace)
    args <- sepBy1 metaValue $ trySpaceSurrounding $ char ','
    void $ many (satisfy isHorizontalSpace) *> char ')'
    let argsArr = fromFoldable args
    pure $ if (A.length argsArr) == 1 then
             (case argsArr of
                [x] -> x
                _ -> PVString "")
           else PVFunc "list" argsArr
  )
  pure $ Property ("@" <> key) { val, meta: [] }

parseMeta :: Parser (Array Property)
parseMeta = try (do
  void $ string "//_"
  void $ many (satisfy isHorizontalSpace)
  attrs <- many (try (many (satisfy isHorizontalSpace) *> metaAttr))
  void $ many (satisfy \c -> c /= '\n')
  pure $ fromFoldable attrs
) <|> pure []

-- --- 4. Property & Block Parsers ---

property :: Parser Property
property = do
  k <- ident
  void $ whitespace *> string "=" *> whitespace
  v <- prismaValue
  m <- (many (satisfy isHorizontalSpace) *> parseMeta)
  pure $ Property k { val: v, meta: m }

blockAttribute :: Parser Property
blockAttribute = do
  void $ string "@@"
  k <- ident
  val <- option (PVString "true") (try parseAttributeArgs)
  m <- (many (satisfy isHorizontalSpace) *> parseMeta)
  pure $ Property ("@@" <> k) { val, meta: m }

parsePropsBody :: Parser (Array Property)
parsePropsBody = do
  void $ whitespace *> string "{" *> whitespace
  props <- many (property <* whitespace)
  void $ string "}"
  pure $ fromFoldable props

-- --- 5. Entity Parsers ---

parseGenerator :: Parser Generator
parseGenerator = do
  void $ string "generator" *> whitespace
  name <- ident
  props <- parsePropsBody
  let
    providerVal = case find (\(Property key _) -> key == "provider") props of
      Just (Property _ { val: PVString s }) -> s
      _ -> ""
    otherProps = filter (\(Property key _) -> key /= "provider") props
  pure { name, provider: providerVal, properties: otherProps }

unProperty :: Property -> MetaValue
unProperty (Property key x) = x

parseDatasource :: Parser Datasource
parseDatasource = do
  void $ string "datasource" *> whitespace
  name <- ident
  props <- parsePropsBody
  let
    providerVal = case find (\(Property key _) -> key == "provider") props of
      Just (Property _ { val: PVString s }) -> s
      _ -> ""
    urlVal = case find (\(Property key _) -> key == "url") props of
      Just (Property _ { val: v }) -> v
      _ -> PVString ""
    otherProps = filter (\(Property key _) -> key /= "provider" && key /= "url") props
  pure { name, provider: providerVal, url: urlVal, properties: otherProps }

parseEnum :: Parser EnumDef
parseEnum = do
  void $ string "enum" *> whitespace
  name <- ident
  void $ whitespace *> string "{" *> whitespace
  items <- many (
             ((Left <$> try blockAttribute) <|>
              (Right <$> (try do
                 n <- ident
                 void $ many (satisfy isHorizontalSpace)
                 attrs <- many (try (parseAttribute <* many (satisfy isHorizontalSpace)))
                 m <- (many (satisfy isHorizontalSpace) *> parseMeta)
                 pure { name: n, attributes: fromFoldable attrs, meta: m }
               ))
             ) <* whitespace)
  void $ string "}"
  let
    itemList = fromFoldable items
    allAttrs  = catMaybes $ map blush itemList
    allValues = catMaybes $ map hush itemList
  pure $ EnumDef name { values: allValues, attributes: allAttrs }

parseModel :: String -> Parser ModelDef
parseModel model = do
  void $ string model *> whitespace
  name <- ident
  void $ whitespace *> string "{" *> whitespace
  items <- many (
             ((Left <$> try blockAttribute) <|>
              (Right <$> parseModelField)
             ) <* whitespace)
  void $ string "}"
  let
    itemList = fromFoldable items
    allAttrs = catMaybes $ map blush itemList
    allFields = catMaybes $ map hush itemList
  pure $ ModelDef name { fields: allFields, attributes: allAttrs }

parseModelField :: Parser ModelField
parseModelField = try do
  name <- ident
  void $ many (satisfy isHorizontalSpace)
  typeName <- ident
  mod <- option "" ((string "?" *> pure "?") <|> (string "[]" *> pure "[]"))
  void $ many (satisfy isHorizontalSpace)

  -- Parse attributes (like @id) allowing spaces
  attrs <- many (try (parseAttribute <* many (satisfy isHorizontalSpace)))

  -- Parse meta comment //_
  m <- parseMeta
  let fieldType = typeFromString mod typeName
  pure { name, typeName, fieldType,  modifier: mod, attributes: fromFoldable attrs, meta: m }

-- --- 6. Root Parser ---

parseSchema :: Parser (Array SchemaItem)
parseSchema = do
  void whitespace
  items <- many $ (
        (ItemGenerator <$> parseGenerator) <|>
        (ItemDatasource <$> parseDatasource) <|>
        (ItemEnum <$> parseEnum) <|>
        (ItemModel <$> parseModel "model") <|>
        (ItemView  <$> parseModel "view")
      ) <* whitespace
  pure $ fromFoldable items

runParser :: String -> Either {error :: String, pos :: Int} (Array SchemaItem)
runParser = StringParser.runParser parseSchema
