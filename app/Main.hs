module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.String.Utils as Utils

{-
We need an entity definition format
We can use a map for it =>
define Person
    id int
    name string
    last_name string
    date_of_birth date
end
===>
{
    entity: Person,
    columns: {
        id: {
            order: 0,
            type: int
        },
        name: {
            order: 1,
            type: string
        },
        last_name: {
            order: 2,
            type: string
        },
        date_of_birth: {
            order: 3,
            type: date
        }
    },
}

database:

Person: [
    {},
    {},
    {},
]

-}



slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

removeFirstLast :: String -> String
removeFirstLast str = slice 1 (length str - 2) str

removeQuotes :: String -> String
removeQuotes str =
    if head str == last str && (head str == '\'' || head str == '"') 
    then removeFirstLast str
    else str

getAllParameters :: String -> [String]
getAllParameters =
    map (removeQuotes . Utils.strip) .
    Utils.split "," .
    removeFirstLast .
    Utils.strip

constructAdd :: String -> Maybe Operation
constructAdd str = Just Add {entity = entityString, values = values}
  where
    entityString = takeWhile (/= '(') $ Utils.strip str
    (_, rest) = splitAt (length entityString) $ Utils.strip str
    values = getAllParameters rest

constructGet :: String -> Maybe Operation
constructGet str = Just Get {entity = entityString} 
    where 
        entityString:_ = Utils.splitWs $ Utils.strip str

constructModify :: String -> Maybe Operation
constructModify str = Just None

parse :: String -> Maybe Operation
parse command = result
  where
    op = takeWhile (/= ' ') $ dropWhile (== ' ') command
    (_, rest) = splitAt (length op) command
    result = case op of
      "add" -> constructAdd rest
      "get" -> constructGet rest
      _ -> Nothing

main :: IO ()
main = do
  putStrLn "alexandra>"
  command <- getLine
  let pieces = Utils.split " " command
  case head pieces of
    "add" -> print $ tail pieces
    _ -> print "Invalid command"