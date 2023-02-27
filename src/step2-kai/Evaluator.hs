module Evaluator
  ( eval ) where

import Interface
  ( throwError
  )
import Type
  ( LispValue (..)
  , Atom (..)
  , LispError ( UnboundVariable ), Error
  )
import Printer ( ) -- Import Show LispValue

eval :: LispValue -> Error LispValue
eval (Atom (Character character)) = return . Atom $ Character character
eval (Atom (Number number)) = return . Atom $ Number number
eval (List [Atom (Symbol "quote"), content]) = return content
eval (List (Atom (Symbol functionName) : arguments)) = do -- Error
  arguments' <- mapM eval arguments
  apply functionName arguments'
eval value = throwError . UnboundVariable $ show value

-- augend addend subtrahend minuend multiplicand multiplier dividend divisor
apply :: String -> [LispValue] -> Error LispValue
apply "+" [Atom (Number augend), Atom (Number addend)] =
  return . Atom . Number $ augend + addend
apply "-" [Atom (Number subtrahend), Atom (Number minuend)] =
  return . Atom . Number $ subtrahend - minuend
apply "*" [Atom (Number multiplicand), Atom (Number multiplier)] =
  return . Atom . Number $ multiplicand * multiplier
apply "/" [Atom (Number dividend), Atom (Number divisor)] =
  return . Atom . Number $ dividend / divisor
