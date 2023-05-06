module Main where

import Interface ( IORef, readline, getArgs )
import Type ( Environment )
import Reader ( read )
import Printer ( print )
import Evaluator ( eval )
import Core ( coreEnvironmentIOIORef )

rep :: IORef Environment -> String -> IO String
rep environmentIORef string = do -- IO
  readed <- return $ Reader.read string
  evaled <- return $ readed >>= eval environmentIORef
  Printer.print evaled

repl :: IORef Environment -> IO ()
repl environmentIORef = do
  line <- readline "user> "
  case line of
    Nothing -> return ()
    Just "" -> repl environmentIORef
    Just string -> (rep environmentIORef string >>= putStrLn) >> repl environmentIORef

main :: IO ()
main = do -- IO
  putStrLn "Welcome to lapse version 1.0.0."
  args <- getArgs
  coreEnvironmentIORef <- coreEnvironmentIOIORef
  let loads = map function args
  mapM (rep coreEnvironmentIORef) loads
  repl coreEnvironmentIORef
  putStrLn "Goodbye."
  where function :: String -> String
        function path =
          "(eval (read (read-from (open-input-file \"" ++
          path ++ "\"))))"
