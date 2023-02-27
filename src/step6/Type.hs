module Type
  ( LispValue (..)
  , Atom (..)
  , LispFunction
  , LispError (..), Error
  , Environment
  ) where

import Interface
  ( ExceptT,
    Rational,
    IORef,
    Handle
  )

type LispFunction = LispValue -> Error LispValue

data Atom = Symbol String
          | Character Char
          | Number Rational
          | Function LispFunction
          | Reference (IORef LispValue)
          | Port Handle
data LispValue = Atom Atom
               | List [LispValue]

data LispError = ParseError String
               | UnboundVariable String
               | TODO String

type Error = ExceptT LispError IO

type Environment = [(LispValue, IORef LispValue)]
