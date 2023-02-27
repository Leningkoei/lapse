module Type
  ( LispVal (..)
  , Atom (..)
  , LispNumber (..)
  , Error, ExceptT, throwError, catchError, runExceptT
  ) where

import Control.Monad.Except

data LispNumber = Integer Integer
                | Float Float
data Atom = Symbol String
          | Character Char
          | Number LispNumber
data LispVal = Atom Atom
             | List [LispVal]

type Error = ExceptT String IO
