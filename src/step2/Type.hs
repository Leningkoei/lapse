module Type
  ( LispValue (..)
  , Atom (..)
  , LispNumber (..)
  , LispError (..), Error
  ) where

import Interface
  ( ExceptT
  )

data LispNumber = Integer Integer
                | Float Float
data Atom = Symbol String
          | Character Char
          | Number LispNumber
data LispValue = Atom Atom
               | List [LispValue]

data LispError = ParseError String
               | UnboundVariable String

type Error = ExceptT LispError IO
