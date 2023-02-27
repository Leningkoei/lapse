module Printer
  ( Printer.print, lispValue2string ) where

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

instance Show LispValue where show = printLispValue
printLispValue :: LispValue -> String
printLispValue (Atom (Symbol symbol)) = symbol
printLispValue (Atom (Number ratio)) =
  let numerator' = numerator ratio
      denominator' = denominator ratio
  in if denominator' == 1
       then show numerator'
       else (show numerator') ++ "/" ++ (show denominator')
printLispValue (Atom (Character character)) = show character
printLispValue (Atom (Function function)) = "function without name"
printLispValue (Atom (Macro function)) = "Macro without name"
printLispValue (Atom (Reference reference)) = "reference"
printLispValue (Atom (Port port)) = "port"
printLispValue (List list) = if checkIsString list
  then show . lispValue2string $ List list
  else "(" ++ (unwords $ map printLispValue list) ++ ")"
  where checkIsString :: [LispValue] -> Bool
        checkIsString [] = True
        checkIsString (Atom (Character head) : rest) = checkIsString rest
        checkIsString _ = False

instance Show LispError where show = printLispError
printLispError :: LispError -> String
printLispError (ParseError error) = error
printLispError (UnboundVariable error) = "unbound variable: " ++ error
printLispError (TODO error) = error

lispValue2string :: LispValue -> String
lispValue2string (List lispCharacterList) = list2string lispCharacterList ""
  where list2string :: [LispValue] -> String -> String
        list2string [] string = reverse string
        list2string (Atom (Character x):xs) string = list2string xs $ x:string
        list2string (x:xs) string = list2string xs $ show x ++ string
lispValue2string other = show other
