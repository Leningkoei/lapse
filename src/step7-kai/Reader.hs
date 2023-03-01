module Reader
  ( Reader.read, string2lispString ) where

import Interface
  ( Parser
  , char, digit, letter
  , many, many1
  , noneOf, oneOf, parse
  , endBy, sepEndBy, skipMany1, space, spaces, string
  , throwError, try
  , (<|>), (%)
  , liftIO
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
parseLispValue =  try parseDereference
              <|> (parseAtom >>= return . Atom)
              <|> parseList
              <|> parseString
              <|> parseQuoteds
              <|> parseReference

spaces1 :: Parser ()
spaces1 = skipMany1 space

symbol :: Parser Char
symbol = oneOf "~!@#$%^*-+=|:<>/?"

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
  character <- escapedChars <|> noneOf "\\'"
  char '\''
  return $ Character character
  where escapedChars = do
          char '\\'
          char <- oneOf "\\'nrt"
          return $ case char of
            '\\' -> '\\'
            '\'' -> '\''
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'

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
  contents <- sepEndBy parseLispValue parseIneffectiveCharacters
  spaces >> char ')'
  return $ List contents

string2lispString :: String -> LispValue
string2lispString = List . map (Atom . Character)

parseString :: Parser LispValue
parseString = do
  char '"'
  value <- many (escapedChars <|> noneOf "\\\"")
  char '"'
  return $ List [Atom $ Symbol "quote", string2lispString value]
  where escapedChars :: Parser Char
        escapedChars = do
          char '\\'
          char <- oneOf "\\\"nrt"
          return $ case char of
            '\\' -> '\\'
            '"' -> '"'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'

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

parseReference :: Parser LispValue
parseReference = do
  char '&'
  content <- parseLispValue
  return $ List [Atom $ Symbol "reference!", content]

parseDereference :: Parser LispValue
parseDereference = do
  char '*'
  content <- parseLispValue
  return $ List [Atom $ Symbol "dereference!", content]

parseComment :: Parser ()
parseComment = do
  char ';'
  skipMany1 $ noneOf "\n\r"
  oneOf "\n\r"
  return ()

parseIneffectiveCharacters :: Parser [()]
parseIneffectiveCharacters = sepEndBy spaces1 parseComment
