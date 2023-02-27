module Evaluator
  ( eval ) where

import Interface
  ( throwError
  , IORef
  )
import Type
  ( LispValue (..)
  , Atom (..)
  , LispError ( UnboundVariable ), Error
  , Environment
  )
import Environment ( get, define )
import Printer ( ) -- Import Show LispValue

eval :: IORef Environment -> LispValue -> Error LispValue
eval _ (Atom (Character character)) = return . Atom $ Character character
eval _ (Atom (Number number)) = return . Atom $ Number number
eval environmentIORef (Atom (Symbol symbol)) = get environmentIORef symbol
eval _ (List [Atom (Symbol "quote"), content]) = return content
eval environmentIORef (List [Atom (Symbol "define!") , Atom (Symbol name) , content]) =
  eval environmentIORef content >>= define environmentIORef name
eval environmentIORef (List (Atom (Symbol functionName) : arguments)) =
  mapM (eval environmentIORef) arguments >>= apply functionName
eval _ value = throwError . UnboundVariable $ show value

-- augend addend subtrahend minuend multiplicand multiplier dividend divisor
apply :: String -> [LispValue] -> Error LispValue
apply "apply" [Atom (Symbol functionName), List arguments] =
  apply functionName arguments
apply "+" [Atom (Number augend), Atom (Number addend)] =
  return . Atom . Number $ augend + addend
apply "-" [Atom (Number subtrahend), Atom (Number minuend)] =
  return . Atom . Number $ subtrahend - minuend
apply "*" [Atom (Number multiplicand), Atom (Number multiplier)] =
  return . Atom . Number $ multiplicand * multiplier
apply "/" [Atom (Number dividend), Atom (Number divisor)] =
  return . Atom . Number $ dividend / divisor

add :: [LispValue] -> Error LispValue
add [Atom (Number augend), Atom (Number addend)] =
  return . Atom . Number $ augend + addend
