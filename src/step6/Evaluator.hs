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
-- import Reader ( load )
import Printer ( lispValue2string ) -- Import Show LispValue

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
-- eval environmentIORef (List [Atom (Symbol "load!"), content]) = do -- Error
--   filename' <- eval environmentIORef content
--   filename <- return $ lispValue2string filename'
--   load filename >>= mapM (eval environmentIORef) >>= return . last
--   -- where list2string :: [LispValue] -> String -> String
--   --       list2string [] string = reverse string
--   --       list2string (Atom (Character x):xs) string = list2string xs $ x:string
--   --       list2string (x:xs) string = list2string xs $ show x ++ string
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
  evaledArguments <- mapM (eval environmentIORef) arguments
  function <- eval environmentIORef functionName
  apply function evaledArguments
eval environmentIORef symbol = get environmentIORef symbol

apply :: LispValue -> [LispValue] -> Error LispValue
apply (Atom (Function function)) (argument:[]) = function argument
apply (Atom (Function function)) (argument:arguments) = do -- Error
  function' <- function argument
  apply function' arguments
apply _ _ = throwError $ TODO "nmsl"
