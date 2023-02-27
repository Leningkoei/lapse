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
import Environment ( get, define, bindVariables )
import Printer ( ) -- Import Show LispValue

eval :: IORef Environment -> LispValue -> Error LispValue
eval environmentIORef (List [Atom (Symbol "eval"), list]) =
  eval environmentIORef list >>= eval environmentIORef
eval _ (Atom (Character character)) = return . Atom $ Character character
eval _ (Atom (Number number)) = return . Atom $ Number number
eval _ (List [Atom (Symbol "quote"), content]) = return content
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
eval environmentIORef (List (functionName : arguments)) = do -- Error
  evaledArguments <- mapM (eval environmentIORef) arguments
  function <- eval environmentIORef functionName
  apply function evaledArguments
eval environmentIORef symbol = get environmentIORef symbol

apply :: LispValue -> [LispValue] -> Error LispValue
apply (Atom (Function function)) (argument:[]) = function argument
apply (Atom (Function function)) (argument:arguments) = do -- Error
  function' <- function argument
  apply function' arguments
