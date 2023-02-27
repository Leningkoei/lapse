module Reader
  ( Reader.read ) where

import Interface
  ( Parser
  , char, digit, letter
  , many, many1
  , noneOf, oneOf, parse
  , sepEndBy, skipMany1, space, spaces, string
  , throwError, try
  , (<|>), (%)
  )
import Type
  ( LispValue (..)
  , Atom (..)
  , LispError ( ParseError ), Error
  )

read :: String -> Error LispValue
read string = case parse parseLispValue' "lisp" string of
  Left error -> throwError . ParseError $ show error
  Right value -> return value
  where parseLispValue' = do
          spaces
          content <- parseLispValue
          spaces
          return content

parseLispValue :: Parser LispValue
parseLispValue =  (parseAtom >>= return . Atom)
            <|> parseList
            <|> parseString
            <|> parseQuoteds

spaces1 :: Parser ()
spaces1 = skipMany1 space

symbol :: Parser Char
symbol = oneOf "~!@#$%^&*-+=|:<>/?"

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  char <- oneOf "\\\"nrt"
  return $ case char of
    '\\' -> '\\'
    '"' -> '"'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'

parseAtom :: Parser Atom
parseAtom =  parseSymbol
         <|> try parseCharacter -- conflict with parse-quoted
         <|> parseNumber

parseSymbol :: Parser Atom
parseSymbol = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return . Symbol $ first : rest

parseCharacter :: Parser Atom
parseCharacter = do
  char '\''
  character <- escapedChars <|> noneOf "\\\""
  char '\''
  return $ Character character

parseNumber :: Parser Atom
parseNumber =  try parseRatio
           <|> try parseFloat
           <|> parseInteger

parseRatio :: Parser Atom
parseRatio = do
  a <- many1 digit
  char '/'
  b <- many1 digit
  numerator <- return $ Prelude.read a
  denominator <- return $ Prelude.read b
  return . Number $ numerator % denominator

parseFloat :: Parser Atom
parseFloat = do
  a <- many1 digit
  char '.'
  b <- many1 digit
  numerator <- return . Prelude.read $ a ++ b
  nmsl <- return $ (fromIntegral $ length b :: Float)
  denominator <- return . round $ 10.0 ** nmsl
  return . Number $ numerator % denominator

parseInteger :: Parser Atom
parseInteger = do
  integerString <- many1 digit
  numerator <- return $ Prelude.read integerString
  return . Number $ numerator % 1

parseList :: Parser LispValue
parseList = do
  char '(' >> spaces
  contents <- sepEndBy parseLispValue spaces1
  spaces >> char ')'
  return $ List contents

parseString :: Parser LispValue
parseString = do
  char '"'
  value <- many (escapedChars <|> noneOf "\\\"")
  char '"'
  return $ List [Atom $ Symbol "quote", List $ map (Atom . Character) value]

parseQuoteds :: Parser LispValue
parseQuoteds =  parseQuoted
            <|> parseQuasiQuoted
            <|> try parseUnQuoteSpliced
            <|> parseUnQuoted

parseQuoted :: Parser LispValue
parseQuoted = do
  char '\''
  content <- parseLispValue
  return $ List [Atom $ Symbol "quote", content]

parseQuasiQuoted :: Parser LispValue
parseQuasiQuoted = do
  char '`'
  content <- parseLispValue
  return $ List [Atom $ Symbol "quasi-quote", content]

parseUnQuoteSpliced :: Parser LispValue
parseUnQuoteSpliced = do
  string ",@"
  content <- parseLispValue
  return $ List [Atom $ Symbol "unquote-splice", content]

parseUnQuoted :: Parser LispValue
parseUnQuoted = do
  char ','
  content <- parseLispValue
  return $ List [Atom $ Symbol "unquote", content]
