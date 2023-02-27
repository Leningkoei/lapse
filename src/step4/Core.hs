module Core
  ( coreEnvironmentIOIORef
  ) where

import Interface ( IORef, throwError, liftIO )
import Type ( LispFunction, LispValue(..), Atom (..), Environment, Error, LispError(..) )
import Environment ( nullEnvironmentIOIORef, bindVariables )

core :: [(LispValue, LispValue)]
core = [ (Atom $ Symbol "apply", Atom $ Function apply)
       , (Atom $ Symbol "print!", Atom $ Function Core.print)
       , (Atom $ Symbol "cons", Atom $ Function cons)
       , (Atom $ Symbol "car", Atom $ Function car)
       , (Atom $ Symbol "cdr", Atom $ Function cdr)
       , (Atom $ Symbol "atom?", Atom $ Function atomp)
       , (Atom $ Symbol "=", Atom $ Function eq)
       , (Atom $ Symbol ">", Atom $ Function greater)
       , (Atom $ Symbol "<", Atom $ Function less)
       , (Atom $ Symbol "+", Atom $ Function add)
       , (Atom $ Symbol "-", Atom $ Function sub)
       , (Atom $ Symbol "*", Atom $ Function mul)
       , (Atom $ Symbol "/", Atom $ Function Core.div)
       ]

coreEnvironmentIOIORef :: IO (IORef Environment)
coreEnvironmentIOIORef = do -- IO
  nullEnvironmentIORef <- nullEnvironmentIOIORef
  bindVariables nullEnvironmentIORef core

apply :: LispFunction
apply [Atom (Function function), List arguments] = function arguments

print :: LispFunction
print [List list] = do -- Error
  liftIO . putStr $ list2string list ""
  return true
  where list2string :: [LispValue] -> String -> String
        list2string [] string = reverse string
        list2string (Atom (Character x):xs) string = list2string xs $ x:string
print [Atom (Character character)] = do
  liftIO . putStr $ character : ""
  return true
print [other] = do
  liftIO . putStr $ show other
  return true

cons :: LispFunction
cons [x, List xs] = return . List $ x : xs
car :: LispFunction
car [List (x:_)] = return x
cdr :: LispFunction
cdr [List (_:xs)] = return $ List xs

atomp :: LispFunction
atomp [Atom _] = return true
atomp _ = return false

true :: LispValue
true = Atom $ Symbol "true"
false :: LispValue
false = List []
eq :: LispFunction
eq [a, b] = if a == b
  then return true
  else return false
greater :: LispFunction
greater [Atom (Function _), _] = throwError $ TODO "> can't receive function!"
greater [_, Atom (Function _)] = throwError $ TODO "> can't receive function!"
greater [a, b] = if a > b
  then return true
  else return false
less :: LispFunction
less [Atom (Function _), _] = throwError $ TODO "< can't receive function!"
less [_, Atom (Function _)] = throwError $ TODO "< can't receive function!"
less [a, b] = if a < b
  then return true
  else return false

add :: LispFunction
add [Atom (Number augend), Atom (Number addend)] =
  return . Atom . Number $ augend + addend
sub :: LispFunction
sub [Atom (Number subtrahend), Atom (Number minuend)] =
  return . Atom . Number $ subtrahend - minuend
mul :: LispFunction
mul [Atom (Number multiplicand), Atom (Number multiplier)] =
  return . Atom . Number $ multiplicand * multiplier
div :: LispFunction
div [Atom (Number dividend), Atom (Number divisor)] =
  return . Atom . Number $ dividend / divisor
