module Evaluator
  ( eval ) where

import Interface
  ( throwError, liftIO
  , IORef
  )
import Type
  ( LispValue (..)
  , Atom (..)
  , LispFunction
  , LispError ( TODO, UnboundVariable ), Error
  , Environment
  )
import Environment
  ( getIORef
  , getValue
  , setReference
  , dereference
  , define
  , bindVariable
  )
import Printer ( lispValue2string ) -- Import Show LispValue

eval :: IORef Environment -> LispValue -> Error LispValue
eval environmentIORef (List [Atom (Symbol "eval"), value]) =
  eval environmentIORef value >>= eval environmentIORef
eval _ (Atom (Character character)) = return . Atom $ Character character
eval _ (Atom (Number number)) = return . Atom $ Number number
eval _ (Atom (Function function)) = return . Atom $ Function function
eval _ (Atom (Macro macro)) = return . Atom $ Macro macro
eval _ (List [Atom (Symbol "quote"), content]) = return content
eval environmentIORef (List [Atom (Symbol "quasi-quote"), form]) = quasiQuote form
  where quasiQuote :: LispValue -> Error LispValue
        quasiQuote (List [Atom (Symbol "unquote"), form]) = eval environmentIORef form
        quasiQuote (List form) = case form of
          [Atom (Symbol "quasi-quote"), _] -> return $ List form
          _ -> mapM quasiQuote form >>= return . List
        quasiQuote other = return other
eval environmentIORef (List [Atom (Symbol "if"), checkForm, thenForm, elseForm]) = do -- Error
  checked <- eval environmentIORef checkForm
  case checked of
    List [] -> eval environmentIORef elseForm
    _ -> eval environmentIORef thenForm
eval environmentIORef (List [Atom (Symbol "or"), first, second]) = do -- Error
  first' <- eval environmentIORef first
  if first' == List []
    then eval environmentIORef second
    else return first'
eval environmentIORef (List [Atom (Symbol "define!") , name , content]) =
  eval environmentIORef content >>= define environmentIORef name
eval environmentIORef (List (Atom (Symbol "do") : body)) =
  mapM (eval environmentIORef) body >>= return . last
eval environmentIORef (List [Atom (Symbol "lambda"), parameter, body]) =
  return . Atom $ Function function
  where function :: LispFunction
        function argument = do -- Error
          closure <- liftIO $ bindVariable environmentIORef (parameter, argument)
          eval closure body
eval environmentIORef (List [Atom (Symbol "macro"), parameter, body]) =
  return . Atom $ Macro function
  where function :: LispFunction
        function argument = do --Error
          closure <- liftIO $ bindVariable environmentIORef (parameter, argument)
          eval closure body
eval environmentIORef (List [Atom (Symbol "reference!"), List list]) =
  (eval environmentIORef $ List list) >>=
  getIORef environmentIORef >>= return . Atom . Reference
eval environmentIORef (List [Atom (Symbol "reference!"), symbol]) =
  getIORef environmentIORef symbol >>= return . Atom . Reference
eval environmentIORef (List [Atom (Symbol "dereference!"), Atom (Reference reference)]) =
  dereference reference
eval environmentIORef (List [Atom (Symbol "dereference!"), list]) = do -- Error
  reference' <- eval environmentIORef list
  case reference' of
    Atom (Reference reference) -> dereference reference
    _ -> throwError . TODO $ "`" ++ show list ++ "`" ++ " is not a reference."
eval environmentIORef (List [Atom (Symbol "set!"), referenceForm, value]) = do --Error
  Atom (Reference reference) <- eval environmentIORef referenceForm
  eval environmentIORef value >>= setReference reference
eval environmentIORef (List (procedureName : arguments)) = do -- Error
  procedure <- eval environmentIORef procedureName
  apply procedure arguments
  where apply :: LispValue -> [LispValue] -> Error LispValue
        apply (Atom (Function function)) (argument:[]) =
          eval environmentIORef argument >>= function
        apply (Atom (Macro macro)) (argument:[]) =
          macro argument >>= eval environmentIORef
        apply (Atom (Function function)) (argument:arguments) = do -- Error
          procedure <- eval environmentIORef argument >>= function
          apply procedure arguments
        apply (Atom (Macro macro)) (argument:arguments) = do -- Error
          procedure <- macro argument >>= eval environmentIORef
          apply procedure arguments
        apply nmsl sb = throwError . TODO $ "Error: " ++ show nmsl ++ " / " ++ show sb
eval environmentIORef symbol = getValue environmentIORef symbol
