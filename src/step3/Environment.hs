module Environment
  ( nullEnvironmentIOIORef
  , get
  , define
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

nullEnvironmentIOIORef :: IO (IORef Environment)
nullEnvironmentIOIORef = newIORef []

isBound :: IORef Environment -> String -> IO Bool
isBound environmentIORef variableName =
  readIORef environmentIORef >>=
  return . maybe False (const True) .
  lookup variableName

get :: IORef Environment -> String -> Error LispValue
get environmentIORef variableName =
  (liftIO $ readIORef environmentIORef) >>=
  maybe (throwError . UnboundVariable $ "Getting an unbound variable: " ++ variableName)
        (liftIO . readIORef) . lookup variableName

set :: IORef Environment -> String -> LispValue -> Error LispValue
set environmentIORef variableName newValue = do -- Error
  environment <- liftIO $ readIORef environmentIORef
  maybeVariableValueIORef <- return $ lookup variableName environment
  variableValueIORef <-
    maybe (throwError . UnboundVariable $ "Setting an unbound variable: " ++ variableName)
          return maybeVariableValueIORef
  liftIO $ writeIORef variableValueIORef newValue
  return newValue

define :: IORef Environment -> String -> LispValue -> Error LispValue
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

bindVariables :: IORef Environment -> [(String, LispValue)] -> IO (IORef Environment)
bindVariables environmentIORef bindings =
  readIORef environmentIORef >>= extendEnvironment bindings >>= newIORef
  where extendEnvironment :: [(String, LispValue)] -> Environment -> IO Environment
        extendEnvironment [] environment = return environment
        extendEnvironment ((name, value) : rest) environment = do -- IO
          valueIORef <- newIORef value
          extendEnvironment rest $ ((name, valueIORef) : environment)
