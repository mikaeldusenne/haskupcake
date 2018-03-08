{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Types where
import GHC.Generics
import Data.Aeson
import Control.Applicative
import qualified Data.Vector as V

import General

newtype PSResources = PSResources [PSR]
  deriving (Show, Generic)

instance FromJSON PSResources

data PSR = PSR {
               predicates :: [PRD],
               logicalOperators :: [String],
               datatypes :: [DTT],
               selectOperations :: [String],
               visualizations :: [String],
               selectFields :: [Field],
               sorts :: [String],
               name :: String,
               relationships :: [String],
               implementation :: String,
               processes :: [String],
               id :: Int,
               joins :: [String]
               }
  deriving (Show, Generic)

justOrEmpty (Just o) = o
justOrEmpty Nothing  = mempty

perhaps k = (justOrEmpty <$> ) . (.:?k)

instance FromJSON PSR where
  parseJSON (Object o) = do
    prdl    <- perhaps "predicates" o
    logical <- perhaps "logicalOperator" o
    datatypes <- perhaps "datatypes" o
    selectOperations <- perhaps "selectOperations" o
    visualizations <- perhaps "visualizations" o
    selectFields <- perhaps "selectFields" o
    sorts <- perhaps "sorts" o
    name <- o .: "name"
    relationships <- perhaps "relationships" o
    implementation <- perhaps "implementation" o
    processes <- perhaps "processes" o
    id <- o .: "id"
    joins <- perhaps "joins" o
    return $ PSR {predicates = prdl,
                  logicalOperators = logical,
                  datatypes = datatypes,
                  selectOperations = selectOperations,
                  visualizations = visualizations,
                  selectFields = selectFields,
                  sorts = sorts,
                  name = name,
                  relationships = relationships,
                  implementation = implementation,
                  processes = processes,
                  id = id,
                  joins = joins
                  }
    

data DTT = DTT { description :: String,
                 name :: String,
                 pattern :: String,
                 typeof :: String}
  deriving (Show, Generic)

instance FromJSON DTT where
  parseJSON (Object o) = do
    description <- perhaps "description" o
    name        <- perhaps "name" o
    pattern     <- perhaps "pattern" o
    typeof      <- perhaps "typeof" o
    return $ DTT description name pattern typeof

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
             
instance FromJSON PRD
instance FromJSON Field
