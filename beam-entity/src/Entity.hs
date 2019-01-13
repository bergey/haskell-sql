{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |

module Entity where

import           Database.Beam

-- TODO type class, associated data, newtype keys

data EntityT table f = Entity
    { entityId :: C f Int
    , entityVal :: table f
    } deriving Generic

type Entity table = EntityT table Identity
deriving instance Show (table Identity) => Show (Entity table)
deriving instance Eq (table Identity) => Eq (Entity table)

instance Beamable table => Beamable (EntityT table)
instance Table table => Table (EntityT table) where
    data PrimaryKey (EntityT table) f = EntityId (C f Int) deriving Generic -- TODO per-table
    primaryKey = EntityId . entityId
instance Beamable (PrimaryKey (EntityT table))
