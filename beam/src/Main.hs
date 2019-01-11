{-# LANGUAGE UndecidableInstances #-}
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
import           Database.PostgreSQL.Simple.FromField
import           Prelude
import           Text.Read as X (readMaybe)

import qualified Data.Vector as V
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
    , meetingTime :: Columnar f (Maybe UTCTime)
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
    { _people :: f (TableEntity PersonT)
    , _tasks :: f (TableEntity TaskT)
    , _meetings :: f (TableEntity MeetingT)
    } deriving Generic
instance Database be TodoDb

db :: DatabaseSettings be TodoDb
db = defaultDbSettings `withDbModification`
    dbModification {
        _tasks = modifyTable (const "tasks") $ tableModification
            { taskOwner = PersonId (fieldNamed "owner") }
        }

runSql :: Connection -> Pg a -> IO a
runSql = runBeamPostgresDebug putStrLn

main :: IO ()
main = do
    conn <- connect (ConnectInfo "localhost" 5432 "postgres" "" "")

    -- runSql conn $ runInsert $ insert (_people db) $
    --     insertValues
    --         [ Person 3 "Jones" 6 11
    --         , Person 4 "', 1, 2, 3, '" 0 99
    --         ]

    -- insert rows with default PK
    runSql conn $ runInsert $ insert (_people db) $
        insertExpressions
            [ Person default_ (val_ "Proudhon") (val_ (-1)) (val_ (-1))
            , Person default_ (val_ "Kropotkin") (val_ 180) (val_ 64)
            ]

    people <- runSql conn $ runSelectReturningList $ select $ all_ (_people db)
    print @[Person] people

    -- select by ID
    m_marx <- runSql conn $ runSelectReturningOne $ select $
        filter_ ((==. 2) . personId) $
        all_ (_people db)
    print m_marx

    -- other where clause
    adults <- runSql conn $ runSelectReturningList $ select $
        filter_ ((>. 10) . personAge) (all_ (_people db))
    print adults

    -- order by, limit
    sorted <- runSql conn $ runSelectReturningList $ select $
        orderBy_ (\u -> (asc_ (personName u), desc_ (personAge u)))
        (limit_ 3 (all_ (_people db)))
    print sorted

    -- aggregate functions - count / min / max - all rows
    count <- runSql conn $ runSelectReturningList $ select $
             aggregate_ (\_ -> as_ @Int countAll_) (all_ (_people db))
    print count
    eldest <- runSql conn $ runSelectReturningOne $ select $
             aggregate_ (max_ . personAge) (all_ (_people db))
    print eldest

    -- aggregate functions - min / max - group by
    countsByName <- runSql conn $ runSelectReturningList $ select $
        aggregate_ (\p -> (group_ (personName p), as_ @Int countAll_))
                          (all_ (_people db))
    print countsByName

    -- joins
    assigned <- runSql conn $ runSelectReturningList $ select $ do
        person <- all_ (_people db)
        task <- all_ (_tasks db)
        -- guard_ (primaryKey person ==. taskOwner task)
        guard_ (taskOwner task `references_` person)
        return (person, task)
    print assigned

    assigned2 <- runSql conn $ runSelectReturningList $ select $ do
        task <- all_ (_tasks db)
        person <- related_ (_people db) (taskOwner task)
        return (person, task)
    print assigned2

    assigned3 <- runSql conn $ runSelectReturningList $ select $ do
        person <- all_ (_people db)
        tasks <- oneToMany_ (_tasks db) taskOwner person
        return (person, tasks)
    putStr "assigned3"
    print assigned3

    -- outer join
    assigned4 <- runSql conn $ runSelectReturningList $ select $
        outerJoin_ (all_ (_people db)) (all_ (_tasks db)) (\(p, t) -> pk p ==. taskOwner t)
    putStr "assigned4 " >> print assigned4

    meetings <- runSql conn $ runSelectReturningList $ select $ all_ (_meetings db)
    putStr "meetings" >> print meetings

    -- let
    --     tasksQ :: Q PgSelectSyntax TodoDb _ (Text, Text)
    --     tasksQ  = do
    --         person <- all_ (_people db)
    --         task <- all_ (_tasks db)
    --         guard_ (taskOwner task `references_` person)
    --         return (personName person, taskDescription task)
    -- dumpSqlSelect $ tasksQ
    -- tasks <- runSql conn $ runSelectReturningList $ select $ tasksQ
    -- print tasks
