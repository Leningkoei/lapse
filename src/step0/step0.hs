module Main where

import Prelude hiding (read, print)
import System.Console.Readline
  ( readline
  )

read :: String -> String
read string = string

eval :: String -> String
eval string = string

print :: String -> String
print string = string

rep :: String -> String
rep = print . eval . read

repl :: IO ()
repl = do
  line <- readline "user> "
  case line of
    Nothing -> return ()
    Just "" -> repl
    Just string -> (putStrLn $ rep string) >> repl

main :: IO ()
main = repl
