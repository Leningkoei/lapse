module Environment
  ( nullEnvironmentIOIORef
  , getIORef, getValue, setReference, dereference
  , define
  , bindVariable
  , bindVariables
  ) where

import Interface
  ( liftIO, throwError
  , IORef, newIORef, readIORef, writeIORef
  , foldM
  )
import Type
  ( LispValue (..)
  , Atom (..)
  , LispError ( UnboundVariable ), Error
  , Environment
  )
import Comparator ( )
import Printer ( )

nullEnvironmentIOIORef :: IO (IORef Environment)
nullEnvironmentIOIORef = newIORef []

isBound :: IORef Environment -> LispValue -> IO Bool
isBound environmentIORef variableName =
  readIORef environmentIORef >>=
  return . maybe False (const True) .
  lookup variableName

getIORef :: IORef Environment -> LispValue -> Error (IORef LispValue)
getIORef environmentIORef variableName =
  (liftIO $ readIORef environmentIORef) >>=
  maybe (throwError . UnboundVariable $ "Getting an unbound variable: " ++ show variableName)
        return . lookup variableName
getValue :: IORef Environment -> LispValue -> Error LispValue
getValue environmentIORef variableName =
  getIORef environmentIORef variableName >>=
  liftIO . readIORef

setReference :: IORef LispValue -> LispValue -> Error LispValue
setReference reference value =
  liftIO $ writeIORef reference value >>
  return value
dereference :: IORef LispValue -> Error LispValue
dereference reference = liftIO $ readIORef reference

set :: IORef Environment -> LispValue -> LispValue -> Error LispValue
set environmentIORef variableName newValue = do -- Error
  environment <- liftIO $ readIORef environmentIORef
  maybeVariableValueIORef <- return $ lookup variableName environment
  variableValueIORef <-
    maybe (throwError . UnboundVariable $ "Setting an unbound variable: " ++ show variableName)
          return maybeVariableValueIORef
  setReference variableValueIORef newValue

bindVariable :: IORef Environment -> (LispValue, LispValue) -> IO (IORef Environment)
bindVariable environmentIORef (name, value) = do
  environment <- readIORef environmentIORef
  valueIORef <- newIORef value
  newEnvironment <- return $ (name, valueIORef) : environment
  newIORef newEnvironment
bindVariables :: IORef Environment -> [(LispValue, LispValue)] -> IO (IORef Environment)
bindVariables environmentIORef bindings = foldM bindVariable environmentIORef bindings

define :: IORef Environment -> LispValue -> LispValue -> Error LispValue
define environmentIORef variableName value = do -- Error
  alreadyDefined <- liftIO $ isBound environmentIORef variableName
  if alreadyDefined
    then set environmentIORef variableName value
    else liftIO $ do
      bindVariable environmentIORef (variableName, value) >>=
        readIORef >>= writeIORef environmentIORef
      return value
