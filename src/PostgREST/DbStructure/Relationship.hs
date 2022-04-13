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
--
-- The order of the relColumns and relForeignColumns should be maintained to get the
-- join conditions right.
--
-- TODO merge relColumns and relForeignColumns to a tuple or Data.Bimap
data Relationship = Relationship
  { relTable          :: Table
  , relColumns        :: [FieldName]
  , relForeignTable   :: Table
  , relForeignColumns :: [FieldName]
  , relCardinality    :: Cardinality
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
  , junColumns1    :: [FieldName]
  , junConstraint2 :: FKConstraint
  , junColumns2    :: [FieldName]
  }
  deriving (Eq, Generic, JSON.ToJSON)

isSelfReference :: Relationship -> Bool
isSelfReference r = relTable r == relForeignTable r
