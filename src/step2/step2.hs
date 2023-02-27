module Main where

import Prelude hiding ( read, print )

import Interface ( readline )
import Type ( LispValue ( List ) )
import Reader ( read )
import Printer ( print )
import Evaluator ( eval )

rep :: String -> IO String
rep string = do -- IO
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
