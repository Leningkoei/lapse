module Evaluator
  ( eval ) where

import Interface
  ( throwError
  )
import Type
  ( LispValue (..)
  , Atom (..)
  , LispNumber (..)
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
apply "+" [Atom (Number (Integer augend)), Atom (Number (Integer addend))] =
  return . Atom . Number . Integer $ augend + addend
apply "+" [Atom (Number (Float augend)), Atom (Number (Float addend))] =
  return . Atom . Number . Float $ augend + addend
apply "+" [Atom (Number (Float augend)), Atom (Number (Integer addend))] =
  let addend' = fromIntegral addend :: Float
  in return . Atom . Number . Float $ augend + addend'
apply "+" [Atom (Number (Integer augend)), Atom (Number (Float addend))] =
  let augend' = fromIntegral augend :: Float
  in return . Atom . Number . Float $ augend' + addend
apply "-" [Atom (Number (Integer subtrahend)), Atom (Number (Integer minuend))] =
  return . Atom . Number . Integer $ subtrahend - minuend
apply "-" [Atom (Number (Float subtrahend)), Atom (Number (Float minuend))] =
  return . Atom . Number . Float $ subtrahend - minuend
apply "-" [Atom (Number (Float subtrahend)), Atom (Number (Integer minuend))] =
  let minuend' = fromIntegral minuend :: Float
  in return . Atom . Number . Float $ subtrahend - minuend'
apply "-" [Atom (Number (Integer subtrahend)), Atom (Number (Float minuend))] =
  let subtrahend' = fromIntegral subtrahend :: Float
  in return . Atom . Number . Float $ subtrahend' - minuend
apply "*" [Atom (Number (Integer multiplicand)), Atom (Number (Integer multiplier))] =
  return . Atom . Number . Integer $ multiplicand * multiplier
apply "/" [Atom (Number (Integer dividend)), Atom (Number (Integer divisor))] =
  let dividend' = fromIntegral dividend :: Float
      divisor' = fromIntegral divisor :: Float
  in return . Atom . Number . Float $ dividend' / divisor'
