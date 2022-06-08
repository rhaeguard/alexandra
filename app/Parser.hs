{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Applicative
import Control.Monad.Identity (Identity)
import ParserModel
import Text.Parsec ((<?>))
import qualified Text.Parsec as Parsec

-- to get rid of the filename param
parse rule text = Parsec.parse rule "(source)" text

---------------------------------------------------------
-- define entity
---------------------------------------------------------

fieldType :: Parsec.Parsec String () ParserModel.DataType
fieldType =
  (Parsec.string "int" >> return ParserModel.DT_Int)
    <|> (Parsec.string "string" >> return ParserModel.DT_String)
    <|> (Parsec.string "date" >> return ParserModel.DT_Date)

entityField :: Parsec.Parsec String () ParserModel.EntityField
entityField = do
    name <- fieldName
    Parsec.spaces
    type' <- fieldType
    return ParserModel.EntityField {
        name = name,
        typeOf = type'
    }

-- [(fieldName => fieldType)]
entityFields :: Parsec.Parsec String () [ParserModel.EntityField]
entityFields = Parsec.sepBy entityField (Parsec.try commaSeparator)

defineEntityExpr :: Parsec.Parsec String () ParserModel.Operation
defineEntityExpr = do
    Parsec.string "define"
    Parsec.spaces
    entityNameStr <- entityName
    Parsec.spaces
    fields <- entityFields
    Parsec.spaces
    Parsec.string "end"
    return ParserModel.Define {
        entity = entityNameStr,
        fields = fields
    }

---------------------------------------------------------
-- add data
---------------------------------------------------------

commaSeparator :: Parsec.Parsec String () ()
commaSeparator = do
  Parsec.spaces
  Parsec.char ','
  Parsec.spaces

readManyAlphaNum :: Parsec.Parsec String () String
readManyAlphaNum = Parsec.many1 Parsec.alphaNum

-- TODO: probably should start with a capital letter and should not start with a number
entityName :: Parsec.Parsec String () String
entityName = readManyAlphaNum

-- TODO: probably should be lowercase and should not start with a number
fieldName :: Parsec.Parsec String () String
fieldName = readManyAlphaNum

numberValue :: Parsec.Parsec String () ParserModel.ValueHolder
numberValue = do
    value <- Parsec.many1 Parsec.digit
    -- pure kind of wraps it into a monad
    pure $ IntValue (read value :: Int)

stringValue :: Parsec.Parsec String () ParserModel.ValueHolder
stringValue = do
  a <- Parsec.string "\""
  b <- concat <$> Parsec.many quotedChar
  c <- Parsec.string "\""
  pure $ ParserModel.StringValue b
  where
    quotedChar =
      Parsec.try (Parsec.string "\\\\")
        <|> Parsec.try (Parsec.string "\\\"")
        <|> ((Parsec.noneOf "\"\n") >>= \x -> return [x])

ignoreValue :: Parsec.Parsec String () ParserModel.ValueHolder
ignoreValue = Parsec.string "_" >> return ParserModel.IgnoreValue

fieldValue :: Parsec.Parsec String () ParserModel.ValueHolder
fieldValue = Parsec.choice [numberValue, stringValue, ignoreValue]

fieldValues :: Parsec.Parsec String () [ParserModel.ValueHolder]
fieldValues = Parsec.sepBy fieldValue commaSeparator

addExpr :: Parsec.Parsec String () ParserModel.Operation
addExpr = do
  Parsec.string "add"
  Parsec.spaces
  entityNameStr <- entityName
  Parsec.char '('
  values <- fieldValues
  Parsec.char ')'
  return ParserModel.Add {
      entity = entityNameStr,
      values = values
  }

---------------------------------------------------------
-- read data
---------------------------------------------------------
logicalOperator :: Parsec.Parsec String () ParserModel.LogicalOperator
logicalOperator = Parsec.choice [
    Parsec.string "and" >> return ParserModel.OpAnd, 
    Parsec.string "or" >> return ParserModel.OpOr]

comparisonOperator :: Parsec.Parsec String () ParserModel.ComparisonOperator
comparisonOperator = Parsec.choice
      [ Parsec.try (Parsec.string ">") >> return ParserModel.OpGreaterThan,
        Parsec.try (Parsec.string ">=") >> return ParserModel.OpGreaterThanEq,
        Parsec.try (Parsec.string "<=") >> return ParserModel.OpLessThanEq,
        Parsec.try (Parsec.string "<") >> return ParserModel.OpLessThan,
        Parsec.try (Parsec.string "==") >> return ParserModel.OpEqual,
        Parsec.string "!="  >> return ParserModel.OpNotEqual
      ]

logicalExpression :: Parsec.Parsec String () ParserModel.Condition
logicalExpression = do
  Parsec.spaces
  fieldNameStr <- fieldName
  Parsec.spaces
  comparisonStr <- comparisonOperator
  Parsec.spaces
  fieldValueStr <- fieldValue
  Parsec.spaces

  pure $ case comparisonStr of
      ParserModel.OpGreaterThan -> ParserModel.GreaterThan fieldNameStr fieldValueStr
      ParserModel.OpGreaterThanEq -> ParserModel.GreaterThanEq fieldNameStr fieldValueStr
      ParserModel.OpLessThan -> ParserModel.LessThan fieldNameStr fieldValueStr
      ParserModel.OpLessThanEq -> ParserModel.LessThanEq fieldNameStr fieldValueStr
      ParserModel.OpEqual -> ParserModel.Equal fieldNameStr fieldValueStr
      ParserModel.OpNotEqual -> ParserModel.NotEqual fieldNameStr fieldValueStr

orConditionExpression :: Parsec.Parsec String () ParserModel.Condition
orConditionExpression = do 
    t <- conditionalTerm
    Parsec.string "or"
    e <- conditionalExpressions
    pure $ ParserModel.Or t e

conditionalExpressions :: Parsec.Parsec String () ParserModel.Condition
conditionalExpressions = Parsec.try orConditionExpression <|> conditionalTerm

andConditionExpression :: Parsec.Parsec String () ParserModel.Condition
andConditionExpression = do
    t <- conditionalFactor
    Parsec.string "and"
    e <- conditionalTerm
    pure $ ParserModel.And t e

conditionalTerm :: Parsec.Parsec String () ParserModel.Condition
conditionalTerm = Parsec.try andConditionExpression <|> conditionalFactor

parenConditionExpression :: Parsec.Parsec String () ParserModel.Condition
parenConditionExpression = do
    Parsec.char '('
    e <- conditionalExpressions
    Parsec.char ')'
    return e

conditionalFactor:: Parsec.Parsec String () ParserModel.Condition
conditionalFactor = Parsec.try parenConditionExpression <|> logicalExpression

logicalExpressions :: Parsec.Parsec String () ParserModel.Condition
logicalExpressions = do
  Parsec.string "where"
  Parsec.spaces
  expressions <- conditionalExpressions
  return expressions

toCondition :: Maybe ParserModel.Condition -> ParserModel.Condition
toCondition Nothing =  ParserModel.NoCondition
toCondition (Just x) = x 

readExpr :: Parsec.Parsec String () ParserModel.Operation
readExpr = do
  Parsec.string "get"
  Parsec.spaces
  entityNameStr <- entityName
  Parsec.optional Parsec.spaces
  conditionExpr <- Parsec.optionMaybe logicalExpressions
  expressions <- pure $ toCondition conditionExpr
  return ParserModel.Get {
      entity = entityNameStr,
      condition = expressions
  }

expr :: Parsec.Parsec String () ParserModel.Operation
expr = defineEntityExpr <|> addExpr <|> readExpr

-- untested
startExpression :: Parsec.Parsec String () [ParserModel.Operation]
startExpression = do
    expressions <- Parsec.many1 expr
    return expressions