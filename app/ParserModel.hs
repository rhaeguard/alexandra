module ParserModel where

data ValueHolder = IntValue Int | StringValue String | DateValue String | IgnoreValue deriving (Eq, Show)

data DataType = DT_Int | DT_String | DT_Date deriving (Eq, Show)

data EntityField = EntityField {name :: String, typeOf :: DataType} deriving (Eq, Show)

data Condition
  = And Condition Condition
  | Or Condition Condition
  | Not Condition
  | GreaterThan String ValueHolder
  | GreaterThanEq String ValueHolder
  | LessThan String ValueHolder
  | LessThanEq String ValueHolder
  | Equal String ValueHolder
  | NotEqual String ValueHolder 
  | NoCondition deriving (Eq, Show)

data LogicalOperator
  = OpAnd
  | OpOr
  | OpNot deriving (Eq, Show)

data ComparisonOperator
  = OpGreaterThan
  | OpGreaterThanEq
  | OpLessThan
  | OpLessThanEq
  | OpEqual
  | OpNotEqual deriving (Eq, Show)

data Operation
  = Define {entity :: String, fields :: [EntityField]}
  | Add {entity :: String, values :: [ValueHolder]}
  | Get {entity :: String, condition :: Condition}
--   | Modify {entity :: String, values :: [String]}
--   | Delete {entity :: String, values :: [String]}
  | None
  deriving (Show)