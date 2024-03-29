module Core
  ( coreEnvironmentIOIORef
  ) where

import Interface
  ( IORef
  , IOMode( ReadMode, WriteMode )
  , throwError
  , liftIO
  , openFile, hClose, hGetLine, hGetContents, hPutStr, hPrint
  , stdin, stdout
  )
import Type ( LispFunction, LispValue(..), Atom (..), Environment, Error, LispError(..) )
import Environment ( nullEnvironmentIOIORef, bindVariables )
import Reader ( read, string2lispString )
import Evaluator ( eval )
import Printer ( lispValue2string )

core :: [(LispValue, LispValue)]
core = [ (Atom $ Symbol "read", Atom $ Function Core.read)
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
       , (Atom $ Symbol "+stdin+", Atom $ Port stdin)
       , (Atom $ Symbol "+stdout+", Atom $ Port stdout)
       , (Atom $ Symbol "open-input-file", Atom . Function $ makePort ReadMode)
       , (Atom $ Symbol "open-output-file", Atom . Function $ makePort WriteMode)
       , (Atom $ Symbol "close-port", Atom . Function $ closePort)
       , (Atom $ Symbol "read-line-from", Atom $ Function lispReadLine)
       , (Atom $ Symbol "read-from", Atom $ Function lispRead)
       , (Atom $ Symbol "write-to", Atom $ Function lispWrite)
       , (Atom $ Symbol "to-string", Atom $ Function lispValue2lispString)
       ]

coreEnvironmentIOIORef :: IO (IORef Environment)
coreEnvironmentIOIORef = do -- IO
  nullEnvironmentIORef <- nullEnvironmentIOIORef
  bindVariables nullEnvironmentIORef core

read :: LispFunction
read string = Reader.read $ lispValue2string string

cons :: LispFunction
cons x = return . Atom $ Function cons'
  where cons' :: LispFunction
        cons' (List xs) = return . List $ x:xs
        cons' other = throwError . TODO $
          "second argument of cons is not a list but a " ++ show other
car :: LispFunction
car (List (x:_)) = return x
cdr :: LispFunction
cdr (List (_:xs)) = return $ List xs

true :: LispValue
true = Atom $ Symbol "true"
false :: LispValue
false = List []

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

-- io

makePort :: IOMode -> LispFunction
makePort iomode content = do -- Error
  filename <- return $ lispValue2string content
  liftIO $ openFile filename iomode >>= return . Atom . Port

closePort :: LispFunction
closePort (Atom (Port port)) = liftIO $ hClose port >> (return . Atom $ Symbol "true")

lispReadLine :: LispFunction
lispReadLine (Atom (Port port)) = (liftIO $ hGetLine port) >>= string2lispString
  where string2lispString :: String -> Error LispValue
        string2lispString string = return . List $ map (Atom . Character) string
lispRead :: LispFunction
lispRead (Atom (Port port)) = (liftIO $ hGetContents port) >>= string2lispString
  where string2lispString :: String -> Error LispValue
        string2lispString string = return . List $ map (Atom . Character) string
lispWrite :: LispFunction
lispWrite (Atom (Port port)) = return . Atom $ Function lispWrite'
  where lispWrite' :: LispFunction
        lispWrite' lispString = do --Error
          liftIO $ hPutStr port $ lispValue2string lispString
          return . Atom $ Symbol "true"

lispValue2lispString :: LispFunction
lispValue2lispString lispValue = return . string2lispString $ show lispValue
