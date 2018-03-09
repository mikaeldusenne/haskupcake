{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
-- |todo, not used yet, no need to parse all json for now
module Types where

import GHC.Generics
import Data.Aeson
import Control.Applicative
import qualified Data.Vector as V

import Utils.General


-- newtype PSResources = PSResources [PSR]
--   deriving (Show, Generic)

-- instance FromJSON PSResources

-- data PSR = PSR {
--                predicates :: [PRD],
--                logicalOperators :: [String],
--                datatypes :: [DTT],
--                selectOperations :: [String],
--                visualizations :: [String],
--                selectFields :: [Field],
--                sorts :: [String],
--                name :: String,
--                relationships :: [String],
--                implementation :: String,
--                processes :: [String],
--                id :: Int,
--                joins :: [String]
--                }
--   deriving (Show, Generic)

-- justOrEmpty (Just o) = o
-- justOrEmpty Nothing  = mempty

-- perhaps k = (justOrEmpty <$> ) . (.:?k)

-- instance FromJSON PSR where
--   parseJSON (Object o) = do
--     prdl    <- perhaps "predicates" o
--     logical <- perhaps "logicalOperator" o
--     datatypes <- perhaps "datatypes" o
--     selectOperations <- perhaps "selectOperations" o
--     visualizations <- perhaps "visualizations" o
--     selectFields <- perhaps "selectFields" o
--     sorts <- perhaps "sorts" o
--     name <- o .: "name"
--     relationships <- perhaps "relationships" o
--     implementation <- perhaps "implementation" o
--     processes <- perhaps "processes" o
--     id <- o .: "id"
--     joins <- perhaps "joins" o
--     return $ PSR {predicates = prdl,
--                   logicalOperators = logical,
--                   datatypes = datatypes,
--                   selectOperations = selectOperations,
--                   visualizations = visualizations,
--                   selectFields = selectFields,
--                   sorts = sorts,
--                   name = name,
--                   relationships = relationships,
--                   implementation = implementation,
--                   processes = processes,
--                   id = id,
--                   joins = joins
--                   }
    

-- data DTT = DTT { description :: String,
--                  name :: String,
--                  pattern :: String,
--                  typeof :: String}
--   deriving (Show, Generic)

-- instance FromJSON DTT where
--   parseJSON (Object o) = do
--     description <- perhaps "description" o
--     name        <- perhaps "name" o
--     pattern     <- perhaps "pattern" o
--     typeof      <- perhaps "typeof" o
--     return $ DTT description name pattern typeof

-- data PRD = PRD {datatypes :: [DTT],
--                 defaultPredicate :: Bool,
--                 description :: String,
--                 displayName :: String,
--                 fields :: [Field],
--                 id :: Int,
--                 name :: String,
--                 paths :: [String]}
--   deriving (Show, Generic)

-- data Field = Field {dataTypes :: [DTT],
--                     description :: String,
--                     id :: Int,
--                     name :: String,
--                     path :: String,
--                     permittedValues :: [String],
--                     relationships :: [String],
--                     required :: Bool}
--   deriving (Show, Generic)
             
-- instance FromJSON PRD
-- instance FromJSON Field






-- ("fields",Array [
--     Object (fromList [
--                ("<dataTypes>"),
--                ("description",String "By Encounter")])])
--                ("id",Number 1.0),
--                ("name",String "By Encounter"),
--                ("path",String "ENOUNTER"),
--                ("permittedValues",Array [String "YES",String "NO"]),
--                ("relationships",Null),
--                ("required",Bool True),

--  = Just (Array [
--              Object (fromList [
--                         ("predicates",Array [
--                             Object (fromList [
--                                        ("<dataTypes>"),
--                                        ("name",String "CONTAINS"),
--                                        ("defaultPredicate",Bool True),
--                                        ("displayName",String "Contains"),
--                                        ("id",Number 1.0),
--                                        ("description",String "Contains value"),
--                                        ("paths",Array []),
--                                        ("<fields>")
--                                        ]),
--                         ("logicalOperators",Array [String "AND"]),
--                         ("<dataTypes>"),
--                         ("selectOperations",Array []),
--                         ("visualizations",Array []),
--                         ("selectFields",Array []),
--                         ("sorts",Array []),
--                         ("name",String "PMSDN-dev"),
--                         ("relationships",Array [String "PARENT"]),
--                         ("implementation",String "i2b2/tranSMART"),
--                         ("processes",Array []),
--                         ("id",Number 1.0),
--                         ("joins",Array [])])])]


-- datatypes = Array [
--   Object (fromList [
--              ("pattern",String "^(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2}):(\\d{2})$"),
--              ("name",String "dateTime"),
--              ("description",String "Date in yyyy-mm-dd hh:mm:ss format. With hours in 24 hour format")])]
