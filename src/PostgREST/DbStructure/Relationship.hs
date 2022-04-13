{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructure.Relationship
  ( Cardinality(..)
  , Relationship(..)
  , Junction(..)
  , isSelfReference
  ) where

import qualified Data.Aeson as JSON

import PostgREST.DbStructure.Table (Table (..))
import PostgREST.DbStructure.Identifiers (FieldName)

import Protolude


-- | Relationship between two tables.
data Relationship = Relationship
  { relTable          :: Table
  , relForeignTable   :: Table
  , relCardinality    :: Cardinality
  , relColumns        :: [(FieldName, FieldName)]
  }
  deriving (Eq, Generic, JSON.ToJSON)

-- | The relationship cardinality
-- | https://en.wikipedia.org/wiki/Cardinality_(data_modeling)
-- TODO: missing one-to-one
data Cardinality
  = O2M FKConstraint -- ^ one-to-many cardinality
  | M2O FKConstraint -- ^ many-to-one cardinality
  | M2M Junction     -- ^ many-to-many cardinality
  deriving (Eq, Generic, JSON.ToJSON)

type FKConstraint = Text

-- | Junction table on an M2M relationship
data Junction = Junction
  { junTable       :: Table
  , junConstraint1 :: FKConstraint
  , junConstraint2 :: FKConstraint
  , junColumns1    :: [(FieldName, FieldName)]
  , junColumns2    :: [(FieldName, FieldName)]
  }
  deriving (Eq, Generic, JSON.ToJSON)

isSelfReference :: Relationship -> Bool
isSelfReference r = relTable r == relForeignTable r
