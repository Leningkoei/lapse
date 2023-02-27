module Core
  ( coreEnvironmentIOIORef
  ) where

import Interface ( IORef, throwError, liftIO )
import Type ( LispFunction, LispValue(..), Atom (..), Environment, Error, LispError(..) )
import Environment ( nullEnvironmentIOIORef, bindVariables )

core :: [(LispValue, LispValue)]
core = [ (Atom $ Symbol "print!", Atom $ Function Core.print)
       , (Atom $ Symbol "cons", Atom $ Function cons)
       , (Atom $ Symbol "car", Atom $ Function car)
       , (Atom $ Symbol "cdr", Atom $ Function cdr)
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

true :: LispValue
true = Atom $ Symbol "true"
false :: LispValue
false = List []

print :: LispFunction
print (List list) = do --Error
  liftIO . putStr $ list2string list ""
  return true
  where list2string :: [LispValue] -> String -> String
        list2string [] string = reverse string
        list2string (Atom (Character x):xs) string = list2string xs $ x:string
print (Atom (Character character)) = do
  liftIO . putStr $ character : ""
  return true
print other = do
  liftIO . putStr $ show other
  return true

cons :: LispFunction
cons x = return . Atom $ Function cons'
  where cons' :: LispFunction
        cons' (List xs) = return . List $ x:xs
car :: LispFunction
car (List (x:_)) = return . Atom $ Symbol "test"
cdr :: LispFunction
cdr (List (_:xs)) = return $ List xs

atomp :: LispFunction
atomp (Atom _) = return true
atomp _ = return false

eq :: LispFunction
eq a = return . Atom $ Function eq'
  where eq' b = if a == b
          then return true
          else return false
greater :: LispFunction
greater (Atom (Function _)) = throwError $ TODO "> can't receive function!"
greater a = return . Atom $ Function greater'
  where greater' :: LispFunction
        greater' (Atom (Function _)) = throwError $ TODO "> can't receive function!"
        greater' b = if a > b
          then return true
          else return false
less :: LispFunction
less (Atom (Function _)) = throwError $ TODO "< can't receive function!"
less a = return . Atom $ Function less'
  where less' :: LispFunction
        less' (Atom (Function _)) = throwError $ TODO "< can't receive function"
        less' b = if a < b
          then return true
          else return false

add :: LispFunction
add (Atom (Number augend)) = return . Atom $ Function add'
  where add' :: LispFunction
        add' (Atom (Number addend)) = return . Atom . Number $ augend + addend
sub :: LispFunction
sub (Atom (Number subtrahend)) = return . Atom $ Function sub'
  where sub' :: LispFunction
        sub' (Atom (Number minuend)) = return . Atom . Number $ subtrahend - minuend
mul :: LispFunction
mul (Atom (Number multiplicand)) = return . Atom $ Function mul'
  where mul' :: LispFunction
        mul' (Atom (Number multiplier)) = return . Atom . Number $ multiplicand * multiplier
div :: LispFunction
div (Atom (Number dividend)) = return . Atom $ Function div'
  where div' :: LispFunction
        div' (Atom (Number divisor)) = return . Atom . Number $ dividend / divisor
