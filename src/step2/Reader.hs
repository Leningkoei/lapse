module Reader
  ( Reader.read ) where

import Interface
  ( Parser
  , char, digit, letter
  , many, many1
  , noneOf, oneOf, parse
  , sepEndBy, skipMany1, space, spaces, string
  , throwError, try
  , (<|>)
  )
import Type
  ( LispValue (..)
  , Atom (..)
  , LispNumber (..)
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
  return . List $ map (Atom . Character) value

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
