module Main where

import Prelude hiding ( read, print )

import Interface ( IORef, readline )
import Type ( LispValue ( List ), Environment )
import Reader ( read )
import Printer ( print )
import Evaluator ( eval )
import Environment ( nullEnvironmentIOIORef )

rep :: IORef Environment -> String -> IO String
rep environmentIORef string = do -- IO
  readed <- return $ read string
  evaled <- return $ readed >>= eval environmentIORef
  print evaled

repl :: IORef Environment -> IO ()
repl environmentIORef = do
  line <- readline "user> "
  case line of
    Nothing -> return ()
    Just "" -> repl environmentIORef
    Just string -> (rep environmentIORef string >>= putStrLn) >> repl environmentIORef

main :: IO ()
main = nullEnvironmentIOIORef >>= repl
