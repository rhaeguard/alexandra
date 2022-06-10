{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad     (unless)
import System.Console.Haskeline as Haskeline

import qualified Data.List as List
import Data.Map (Map)
import Data.Foldable (for_)
import qualified Data.Map as Map
import qualified Data.String.Utils as Utils

import Text.Parsec.Error

import Parser
import ParserModel

showEntityField :: ParserModel.EntityField -> String
showEntityField value = "\t" ++ name value ++ ": " ++ show (typeOf value) ++ "\n"

showEntity :: (String, [ParserModel.EntityField]) -> String
showEntity (key, values) = 
  "Table: " ++ key ++ "\n" ++ foldr ((\x acc -> acc ++ x) . showEntityField) "" values

looper :: Map.Map String [ParserModel.EntityField] -> InputT IO ()
looper abc = do
  maybeCommand <- Haskeline.getInputLine "alexandra> "
  case maybeCommand of
    Nothing -> do Haskeline.outputStrLn "No input provided, exiting..."
    Just "quit" -> do Haskeline.outputStrLn "Exit"
    Just "show db" -> do
      for_ (Map.toList abc) (Haskeline.outputStrLn . showEntity)
      looper abc
    Just command -> do
      let result = Parser.parse Parser.expr command
      k <- case result of
                Left error -> do
                  Haskeline.outputStrLn "Error occurred: "
                  Haskeline.outputStrLn $ show error
                  pure $ abc
                Right op -> do
                  let entityDatabase = Map.insert (entity op) (fields op) abc
                  Haskeline.outputStrLn "Success"
                  -- Haskeline.outputStrLn $ show op
                  pure $ entityDatabase
      looper k

main :: IO ()
main = Haskeline.runInputT Haskeline.defaultSettings (looper Map.empty)