module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.String.Utils as Utils

import Text.Parsec.Error

import Parser
import ParserModel

main :: IO ()
main = do
  putStrLn "alexandra>"
  command <- getLine
  case command of 
    "quit" -> putStrLn "Exit"
    _ -> do 
      result <- pure $ (Parser.parse Parser.expr command)
      print result
      main