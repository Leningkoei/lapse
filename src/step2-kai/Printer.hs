module Printer
  ( Printer.print ) where

import Prelude hiding ( print )

import Interface
  ( runExceptT
  , numerator, denominator
  )
import Type
  ( LispValue (..)
  , Atom (..)
  , LispError (..), Error
  )

print :: Error LispValue -> IO String
print valueWithError = runExceptT valueWithError >>= return . handleError
  where handleError :: Either LispError LispValue -> String
        handleError valueWithError =
          case valueWithError of
            Left error -> show error
            Right value -> show value

printLispValue :: LispValue -> String
printLispValue (Atom (Symbol symbol)) = symbol
printLispValue (Atom (Number ratio)) =
  let numerator' = numerator ratio
      denominator' = denominator ratio
  in if denominator' == 1
       then show numerator'
       else (show numerator') ++ "/" ++ (show denominator')
printLispValue (Atom (Character character)) = show character
printLispValue (List list) = if checkIsString list
  then show $ list2string list ""
  else "(" ++ (unwords $ map printLispValue list) ++ ")"
  where checkIsString :: [LispValue] -> Bool
        checkIsString [] = True
        checkIsString (Atom (Character head) : rest) = checkIsString rest
        checkIsString _ = False
        list2string :: [LispValue] -> String -> String
        list2string [] accumulator = reverse accumulator
        list2string (Atom (Character character) : rest) accumulator =
          list2string rest $ character : accumulator

instance Show LispValue where show = printLispValue

printLispError :: LispError -> String
printLispError (ParseError error) = error
printLispError (UnboundVariable error) = "unbound variable: " ++ error
instance Show LispError where show = printLispError
