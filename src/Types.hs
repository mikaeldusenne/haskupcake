{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
module Types where
import GHC.Generics

newtype PSResources = PSResources [PSR]
  deriving (Show, Generic)

data PSR = PSR {
               datatypes :: [DTT],
               id :: Int,
               implementation :: String,
               joins :: [String],
               logicalOperators :: [String],
               name :: String,
               predicates :: [PRD]}
  deriving (Show, Generic)

data DTT = DTT { description :: String,
                 name :: String,
                 pattern :: String}
  deriving (Show, Generic)

data PRD = PRD {datatypes :: [DTT],
                defaultPredicate :: Bool,
                description :: String,
                displayName :: String,
                fields :: [Field],
                id :: Int,
                name :: String,
                paths :: [String]}
  deriving (Show, Generic)

data Field = Field {dataTypes :: [DTT],
                    description :: String,
                    id :: Int,
                    name :: String,
                    path :: String,
                    permittedValues :: [String],
                    relationships :: [String],
                    required :: Bool}
  deriving (Show, Generic)
             
