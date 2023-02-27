module Main where

import Prelude hiding ( read, print )
import System.Console.Readline
  ( readline
  )

import Type ( LispVal, Error )
import Reader ( read )
import Printer ( print )

eval :: LispVal -> Error LispVal
eval lispVal = return lispVal

rep :: String -> IO String
rep string = do
  readed <- return $ read string
  evaled <- return $ readed >>= eval
  print evaled

repl :: IO ()
repl = do
  line <- readline "user> "
  case line of
    Nothing -> return ()
    Just "" -> repl
    Just string -> (rep string >>= putStrLn) >> repl

main :: IO ()
main = repl
