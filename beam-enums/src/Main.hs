{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Enums

import           Data.Aeson as JSON
import           Data.Coerce
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Data.Time
import           Data.UUID (UUID)
import           Data.Vector (Vector)
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Prelude

-- import qualified Data.Vector as V
import qualified Data.UUID as UUID

mkID :: Coercible UUID.UUID id => String -> id
mkID = coerce . fromJust . UUID.fromString

data MeetingType = Slack | Phone | Face
    deriving (Eq, Show, Read, Enum, Bounded, Generic)

instance FromField MeetingType where
    fromField = fromFieldEnum "meeting_type"
instance ToField MeetingType where
    toField = toFieldEnum
-- instance ToField MeetingType where
--     toField = toFieldShow
instance HasSqlValueSyntax PgValueSyntax MeetingType where
    sqlValueSyntax = sqlFieldValueSyntax
instance FromBackendRow Postgres MeetingType

data MeetingT f = Meeting
    { meetingId :: Columnar f UUID
    , meetingTime :: Columnar f (Maybe UTCTime)
    , meetingType :: C f MeetingType
    , meetingDetails :: Columnar f JSON.Value
    , meetingAttendees :: Columnar f (Vector Text)
    } deriving Generic

type Meeting = MeetingT Identity
type MeetingId = PrimaryKey MeetingT Identity

deriving instance Show Meeting
deriving instance Eq (PrimaryKey MeetingT Identity)
deriving instance Eq Meeting

instance Beamable MeetingT
instance Table MeetingT where
    data PrimaryKey MeetingT f = MeetingId (Columnar f UUID) deriving Generic
    primaryKey = MeetingId . meetingId
instance Beamable (PrimaryKey MeetingT)

data TodoDb f = TodoDB
    { _meetings :: f (TableEntity MeetingT)}
    deriving Generic
instance Database be TodoDb

db :: DatabaseSettings be TodoDb
db = defaultDbSettings

runLabeled :: Show a => Connection -> String -> Pg a -> IO ()
runLabeled conn label query = do
    putStrLn label
    a <- runBeamPostgresDebug putStrLn conn query
    print a
    putStrLn ""

main :: IO ()
main = do
    conn <- connect (ConnectInfo "localhost" 5432 "postgres" "" "")
    let
        runSql :: Show a => String -> Pg a -> IO ()
        runSql = runLabeled conn

    now <- liftIO getCurrentTime
    runSql "insert with default PK" $ runInsert $ insert (_meetings db) $
        insertExpressions
            [ Meeting default_ (val_ (Just now)) (val_ Face) default_ (array_ [val_ "Daniel", val_ "Jones"])
            , Meeting default_ (val_ (Just now)) (val_ Phone) (val_ Null) (array_ [val_ "Daniel", val_ "Marx"])
            ]

    runSql "array column" $ runSelectReturningList $ select $ all_ (_meetings db)

    runSql "array @> superset" $ runSelectReturningList $ select $ do
        m <- all_ (_meetings db)
        guard_ (meetingAttendees m `isSupersetOf_` array_ ["Marx"])
        return m
