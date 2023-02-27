module Type
  ( LispValue (..)
  , Atom (..)
  , LispError (..), Error
  , Environment
  ) where

import Interface
  ( ExceptT,
    Rational,
    IORef
  )

data Atom = Symbol String
          | Character Char
          | Number Rational
data LispValue = Atom Atom
               | List [LispValue]

data LispError = ParseError String
               | UnboundVariable String

type Error = ExceptT LispError IO

type Environment = [(String, IORef LispValue)]
