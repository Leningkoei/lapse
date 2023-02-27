module Printer
  ( Printer.print ) where

import Prelude hiding ( print )

import Type
  ( LispVal (..)
  , Atom (..)
  , LispNumber (..)
  , Error, ExceptT, runExceptT
  )

print :: Error LispVal -> IO String
print valueWithError = runExceptT valueWithError >>= return . handleError
  where handleError :: Either String LispVal -> String
        handleError valueWithError =
          case valueWithError of
            Left error -> error
            Right value -> show value

print' :: LispVal -> String
print' (Atom (Symbol symbol)) = symbol
print' (Atom (Number (Integer integer))) = show integer
print' (Atom (Number (Float float))) = show float
print' (Atom (Character character)) = show character
print' (List list) = if checkIsString list
  then show $ list2string list ""
  else "(" ++ (unwords $ map print' list) ++ ")"
  where checkIsString :: [LispVal] -> Bool
        checkIsString [] = True
        checkIsString (Atom (Character head) : rest) = checkIsString rest
        checkIsString _ = False
        list2string :: [LispVal] -> String -> String
        list2string [] accumulator = reverse accumulator
        list2string (Atom (Character character) : rest) accumulator =
          list2string rest $ character : accumulator

instance Show LispVal where show = print'
-- type Test = Error LispVal
-- instance Show Test where show = print
