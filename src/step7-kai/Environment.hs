module Environment
  ( nullEnvironmentIOIORef
  , get, getIORef, setIORef, dereference
  , define
  , bindVariables
  ) where

import Interface
  ( liftIO, throwError
  , IORef, newIORef, readIORef, writeIORef
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

get :: IORef Environment -> LispValue -> Error LispValue
get environmentIORef variableName =
  (liftIO $ readIORef environmentIORef) >>=
  maybe (throwError . UnboundVariable $ "Getting an unbound variable: " ++ show variableName)
        (liftIO . readIORef) . lookup variableName
getIORef :: IORef Environment -> LispValue -> Error (IORef LispValue)
getIORef environmentIORef variableName =
  (liftIO $ readIORef environmentIORef) >>=
  maybe (throwError . UnboundVariable $ "Getting an unbound variable: " ++ show variableName)
        return . lookup variableName
setIORef :: IORef LispValue -> LispValue -> Error LispValue
setIORef reference value =
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
  liftIO $ writeIORef variableValueIORef newValue
  return newValue

define :: IORef Environment -> LispValue -> LispValue -> Error LispValue
define environmentIORef variableName value = do -- Error
  alreadyDefined <- liftIO $ isBound environmentIORef variableName
  if alreadyDefined
    then set environmentIORef variableName value
    else liftIO $ do -- IO
      environment <- readIORef environmentIORef
      variableValueIORef <- newIORef value
      writeIORef environmentIORef $ (variableName, variableValueIORef) : environment
      -- readIORef environmentIORef >>= mapM (\ (variableName, _) -> putStrLn variableName)
      return value

bindVariables :: IORef Environment -> [(LispValue, LispValue)] -> IO (IORef Environment)
bindVariables environmentIORef bindings =
  readIORef environmentIORef >>= extendEnvironment bindings >>= newIORef
  where extendEnvironment :: [(LispValue, LispValue)] -> Environment -> IO Environment
        extendEnvironment [] environment = return environment
        extendEnvironment ((name, value) : rest) environment = do -- IO
          valueIORef <- newIORef value
          extendEnvironment rest $ ((name, valueIORef) : environment)
