module Main where

import Interface ( IORef, readline )
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
main = coreEnvironmentIOIORef >>= repl
