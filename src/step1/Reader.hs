module Reader
  ( Reader.read ) where

import Text.ParserCombinators.Parsec
  ( Parser
  , char, digit, letter
  , many, many1
  , noneOf, oneOf, parse
  , sepEndBy, skipMany1, space, spaces, string
  , try
  , (<|>)
  )

import Type
  ( LispVal (..)
  , Atom (..)
  , LispNumber (..)
  , Error, throwError, catchError
  )

read :: String -> Error LispVal
read string = case parse parseLispVal' "lisp" string of
  Left error -> throwError $ "No match: " ++ show error
  Right value -> return value
  where parseLispVal' = do
          spaces
          content <- parseLispVal
          spaces
          return content

parseLispVal :: Parser LispVal
parseLispVal =  (parseAtom >>= return . Atom)
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
         <|> try parseCharacter
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
parseNumber = (try parseFloat
           <|> parseInteger) >>= return . Number

parseFloat :: Parser LispNumber
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return . Float . Prelude.read $ x ++ "." ++ y

parseInteger :: Parser LispNumber
parseInteger = do
  integer <- many1 digit
  return . Integer $ Prelude.read integer

parseList :: Parser LispVal
parseList = do
  char '(' >> spaces
  contents <- sepEndBy parseLispVal spaces1
  spaces >> char ')'
  return $ List contents

parseString :: Parser LispVal
parseString = do
  char '"'
  value <- many (escapedChars <|> noneOf "\\\"")
  char '"'
  return . List $ map (Atom . Character) value

parseQuoteds :: Parser LispVal
parseQuoteds =  parseQuoted
            <|> parseQuasiQuoted
            <|> try parseUnQuoteSpliced
            <|> parseUnQuoted

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  content <- parseLispVal
  return $ List [Atom $ Symbol "quote", content]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  content <- parseLispVal
  return $ List [Atom $ Symbol "quasi-quote", content]

parseUnQuoteSpliced :: Parser LispVal
parseUnQuoteSpliced = do
  string ",@"
  content <- parseLispVal
  return $ List [Atom $ Symbol "unquote-splice", content]

parseUnQuoted :: Parser LispVal
parseUnQuoted = do
  char ','
  content <- parseLispVal
  return $ List [Atom $ Symbol "unquote", content]
