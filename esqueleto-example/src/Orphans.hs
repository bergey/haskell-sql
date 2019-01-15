{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orphans where

import           Database.Persist
import           Database.Persist.Postgresql
-- import           Database.Persist.TH

-- import           Control.Arrow ((&&&))
-- import           Control.Monad.IO.Class
-- import           Control.Monad.Logger
import           Data.Aeson (FromJSON(..), ToJSON(..))
-- import           Data.Text (Text)
-- import           Data.Time (UTCTime)
import           Data.UUID (UUID)
import           Prelude
import           Web.PathPieces

import qualified Data.Aeson as JSON
-- import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.UUID as UUID
-- import qualified Data.Vector as V

instance PersistField UUID where
  toPersistValue = PersistDbSpecific . UUID.toASCIIBytes
  fromPersistValue (PersistDbSpecific uuid) =
    case UUID.fromASCIIBytes uuid of
      Nothing -> Left $ "Model/CustomTypes.hs: Failed to deserialize a UUID; received: " <> T.pack (show uuid)
      Just uuid' -> Right uuid'
  fromPersistValue x = Left $ "Model/CustomTypes.hs: When trying to deserialize a UUID: expected PersistDbSpecific, received: " <> T.pack (show x)

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  toPathPiece = UUID.toText
  fromPathPiece = UUID.fromText

-- From
-- https://github.com/creichert/persistent-postgresql-json/blob/master/Database/Persist/Postgresql/Json.hs
-- The above isn't on Hackage, but also doesn't make to / from
-- symmetric in the instances

-- | instance will use Postgresql JSON type (not JSONB)
newtype Json
  = Json JSON.Value
    deriving (Show , Eq , FromJSON , ToJSON )


instance PersistField Json where
  toPersistValue v = PersistByteString $ BSL.toStrict $ JSON.encode v
  fromPersistValue (PersistByteString v)
    = case JSON.decode (BSL.fromStrict v) of
        Nothing -> Left "Invalid JSON"
        Just j  -> Right j
  fromPersistValue _ = Left "Not PersistByteString"

instance PersistFieldSql Json where
   sqlType _ = SqlOther "JSON"

-- | instance will use Postgresql JSONB type (not JSON)
newtype Jsonb
  = Jsonb JSON.Value
    deriving (Show , Eq , FromJSON , ToJSON )

instance PersistField Jsonb where
  toPersistValue v = PersistByteString $ BSL.toStrict $ JSON.encode v
  fromPersistValue (PersistByteString v)
    = case JSON.decode (BSL.fromStrict v) of
        Nothing -> Left "Invalid jsonb"
        Just j  -> Right j
  fromPersistValue _ = Left "Invalid PersistValue for JSONB. PersistByteString required."

instance PersistFieldSql Jsonb where
   sqlType _ = SqlOther "JSONB"
