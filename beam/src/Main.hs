{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
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

import           Data.Coerce
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Data.Time
import           Data.UUID (UUID)
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Postgres
import           Prelude

import           Data.Aeson as JSON

import qualified Data.UUID as UUID

mkID :: Coercible UUID.UUID id => String -> id
mkID = coerce . fromJust . UUID.fromString

data PersonT f = Person
    { personId :: Columnar f Int
    , personName :: Columnar f Text
    , personAge :: Columnar f Int
    , personHeightInches :: Columnar f Double
    } deriving Generic

type Person = PersonT Identity
type PersonId = PrimaryKey PersonT Identity

deriving instance Show Person
deriving instance Show PersonId
deriving instance Eq (PrimaryKey PersonT Identity)
deriving instance Eq Person

instance Beamable PersonT
instance Table PersonT where
    data PrimaryKey PersonT f = PersonId (Columnar f Int) deriving Generic
    primaryKey = PersonId . personId
instance Beamable (PrimaryKey PersonT)

data TaskT f = Task
    { taskId :: C f Int
    , taskOwner :: PrimaryKey PersonT f
    , taskDescription :: C f Text
    } deriving Generic

type Task = TaskT Identity
type TaskId = PrimaryKey TaskT Identity

deriving instance Show Task
deriving instance Show TaskId
deriving instance Eq (PrimaryKey TaskT Identity)
deriving instance Eq Task

instance Beamable TaskT
instance Table TaskT where
    data PrimaryKey TaskT f = TaskId (C f Int) deriving Generic
    primaryKey = TaskId . taskId
instance Beamable (PrimaryKey TaskT)

data MeetingT f = Meeting
    { meetingId :: Columnar f UUID
    , meetingTime :: Columnar f UTCTime
    , meetingDetails :: Columnar f JSON.Value
    , meetingAttendees :: Columnar f [PersonId]
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
    { _people :: f (TableEntity PersonT)
    , _tasks :: f (TableEntity TaskT)
    , _meetings :: f (TableEntity MeetingT)
    } deriving Generic
instance Database be TodoDb

db :: DatabaseSettings be TodoDb
db = defaultDbSettings

runSql :: Connection -> Pg a -> IO a
runSql = runBeamPostgresDebug putStrLn

main :: IO ()
main = do
    conn <- connect (ConnectInfo "localhost" 5432 "postgres" "" "")
    -- runSql conn $
        -- runInsert $ insert (_people db) $ insertValues
        --     [ -- Person 3 "Jones" 6 11
        --      Person 4 "', 1, 2, 3, '" 0 99
        --     ]

    people <- runSql conn $ runSelectReturningList $ select $
        all_ (_people db)
    print @[Person] people

    m_marx <- runSql conn $ runSelectReturningOne $ select $
        filter_ ((==. 2) . personId) $
        all_ (_people db)
    print m_marx

    let
        tasksQ :: Q PgSelectSyntax TodoDb _ (Text, Text)
        tasksQ  = do
            person <- all_ (_people db)
            task <- all_ (_tasks db)
            guard_ (taskOwner task `references_` person)
            return (personName person, taskDescription task)
    dumpSqlSelect $ tasksQ
    tasks <- runSql conn $ runSelectReturningList $ select $ tasksQ
    print tasks

    return ()
