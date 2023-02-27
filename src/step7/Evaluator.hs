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
import Environment ( get, getIORef, setIORef, dereference, define, bindVariables )
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
          -- case form of
          --   (List [Atom (Symbol "unquote"), _]) -> return form
          --   _ -> eval environmentIORef form
        quasiQuote (List form) = case form of
          [Atom (Symbol "quasi-quote"), _] -> return $ List form
          _ -> mapM quasiQuote form >>= return . List
        quasiQuote other = return other
eval environmentIORef (List [Atom (Symbol "if"), checkForm, thenForm, elseForm]) = do -- Error
  checked <- eval environmentIORef checkForm
  case checked of
    List [] -> eval environmentIORef elseForm
    _ -> eval environmentIORef thenForm
eval environmentIORef (List [Atom (Symbol "define!") , name , content]) =
  eval environmentIORef content >>= define environmentIORef name
eval environmentIORef (List (Atom (Symbol "do") : body)) =
  mapM (eval environmentIORef) body >>= return . last
eval environmentIORef (List [Atom (Symbol "lambda"), parameter, body]) =
  return . Atom $ Function function
  where function :: LispFunction
        function argument = do -- Error
          closure <- liftIO $ bindVariables environmentIORef [(parameter, argument)]
          eval closure body
eval environmentIORef (List [Atom (Symbol "macro"), parameter, body]) =
  return . Atom $ Macro function
  where function :: LispFunction
        function argument = do --Error
          closure <- liftIO $ bindVariables environmentIORef [(parameter, argument)]
          eval closure body >>= eval environmentIORef
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
  eval environmentIORef value >>= setIORef reference
eval environmentIORef (List (functionName : arguments)) = do -- Error
  function <- eval environmentIORef functionName
  case function of
    (Atom (Function _)) ->
      mapM (eval environmentIORef) arguments >>=
      apply function
    (Atom (Macro _)) -> apply function arguments
    other -> throwError . TODO $ "not function: " ++ show other
eval environmentIORef symbol = get environmentIORef symbol

apply :: LispValue -> [LispValue] -> Error LispValue
apply (Atom (Function function)) (argument:[]) = function argument
apply (Atom (Function function)) (argument:arguments) = do -- Error
  function' <- function argument
  apply function' arguments
apply (Atom (Macro function)) (argument:[]) = function argument
apply (Atom (Macro function)) (argument:arguments) = do --Error
  function' <- function argument
  apply function' arguments
apply _ _ = throwError $ TODO "nmsl"
