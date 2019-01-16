{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
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
-- import           Text.Read as X (readMaybe)

-- import qualified Data.Vector as V
import qualified Data.UUID as UUID
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

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

data MeetingType = Slack | Phone | Face
    deriving (Eq, Show, Read, Enum, Bounded, Generic)

-- TODO some Generic code to construct instances like these
instance FromField MeetingType where
    fromField field m_val = typename field >>= \case
        "meeting_type" ->
              case m_val of
                  Nothing -> returnError UnexpectedNull field ""
                  Just v  -> case v of
                      "Slack" -> pure Slack
                      "Phone" -> pure Phone
                      "Face" -> pure Face
                      _ -> returnError ConversionFailed field (ellipsis v)
        _ ->  returnError Incompatible field ""

instance ToField MeetingType where
    toField Slack = Plain "Slack"
    toField Phone = Plain "Phone"
    toField Face = Plain "Face"

instance HasSqlValueSyntax PgValueSyntax MeetingType where
    sqlValueSyntax = PgValueSyntax . pgBuildAction . pure . toField
instance FromBackendRow Postgres MeetingType

ellipsis :: BS.ByteString -> String
ellipsis x
    | BS.length x > 25 = take 20 (BSC.unpack x) ++ "[...]"
    | otherwise       = BSC.unpack x

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

    -- runSql "insert full" $ runInsert $ insert (_people db) $
    --     insertValues
    --         [ Person 3 "Jones" 6 11
    --         , Person 4 "', 1, 2, 3, '" 0 99
    --         ]

    runSql "insert with default PK" $ runInsert $ insert (_people db) $
        insertExpressions
            [ Person default_ (val_ "Proudhon") (val_ (-1)) (val_ (-1))
            , Person default_ (val_ "Kropotkin") (val_ 180) (val_ 64)
            ]

    runSql "select all" $ runSelectReturningList $ select $ all_ (_people db)

    runSql "select by ID short" $ runSelectReturningOne $
        lookup_ (_people db) (PersonId 2)

    runSql "select by ID where clause" $ runSelectReturningOne $ select $
        filter_ ((==. 2) . personId) $ all_ (_people db)

    runSql "select by other where clause" $ runSelectReturningList $ select $
        filter_ ((>. 10) . personAge) (all_ (_people db))

    runSql "return only selected fields" $ runSelectReturningList $ select $ do
        p <- all_ (_people db)
        return (personName p, personAge p)

    let byAgeRange (lo, hi) = runSelectReturningList $ select $ do
            p <- all_ (_people db)
            guard_ (personAge p >=. val_ lo &&. personAge p <=. val_ hi)
            return p
    runSql "parameterize by multiple inputs" $ byAgeRange (10, 19)

    runSql "order by & limit" $ runSelectReturningList $ select $
        orderBy_ (\u -> (asc_ (personName u), desc_ (personAge u)))
        (limit_ 3 (all_ (_people db)))

    -- -- aggregate functions - count / min / max - all rows
    runSql "aggregate count(*)" $ runSelectReturningList $ select $
             aggregate_ (\_ -> as_ @Int countAll_) (all_ (_people db))

    runSql "aggregate max" $ runSelectReturningOne $ select $
             aggregate_ (max_ . personAge) (all_ (_people db))

    -- aggregate functions - min / max - group by
    runSql "aggregate group by" $ runSelectReturningList $ select $
        aggregate_ (\p -> (group_ (personName p), as_ @Int countAll_))
                          (all_ (_people db))

    -- -- joins
    runSql "join 1 cross & where" $ runSelectReturningList $ select $ do
        person <- all_ (_people db)
        task <- all_ (_tasks db)
        -- guard_ (primaryKey person ==. taskOwner task)
        guard_ (taskOwner task `references_` person)
        return (person, task)

    runSql "join 2 many first" $ runSelectReturningList $ select $ do
        task <- all_ (_tasks db)
        person <- related_ (_people db) (taskOwner task)
        return (person, task)

    runSql "join 3 one first" $ runSelectReturningList $ select $ do
        person <- all_ (_people db)
        tasks <- oneToMany_ (_tasks db) taskOwner person
        return (person, tasks)

    runSql "outer join" $ runSelectReturningList $ select $
        outerJoin_ (all_ (_people db)) (all_ (_tasks db)) (\(p, t) -> pk p ==. taskOwner t)

    runSql "array column" $ runSelectReturningList $ select $ all_ (_meetings db)

    -- inline SQL - escape hatch, likely needed for some of below
    -- "SELECT name || ' needs to ' || description FROM people JOIN tasks ON owner = people.id"
    runSql "inline SQL || string concat"  $ runSelectReturningList $ select $ do
        p <- all_ (_people db)
        t <- oneToMany_ (_tasks db) taskOwner p
        -- return (p, t)
        return $ as_ @Text $ valueExpr_ $ customExpr_ (\name desc -> name <> "|| ' needs to ' ||" <> desc) (personName p) (taskDescription t)

    runSql "inline SQL modulo 2" $ runSelectReturningList $ select $ do
        p <- all_ (_people db)
        guard_ (customExpr_ (\age -> "mod(" <> age <> ", 2) = 0") (personAge p))
        return p

    runSql "aggregate array_agg 1" $ runSelectReturningList $ select $ do
        p <- all_ (_people db)
        (owner, tasks) <- aggregate_ (\t -> (group_ (taskOwner t), pgArrayAgg (taskDescription t))) (all_ (_tasks db))
        guard_ (pk p ==. owner)
        return (p, tasks)

    -- The version above joins people with a subquery, and uses a
    -- WHERE clause instead of an ON clause.  The version below joins
    -- people and tasks, then groups the whole query - closer to the
    -- SQL I would write by hand.
    runSql "aggregate array_agg 2" $ runSelectReturningList $ select $
        aggregate_ (\(p, t) -> (group_ p, pgArrayAgg (taskDescription t))) $ do
            person <- all_ (_people db)
            tasks <- oneToMany_ (_tasks db) taskOwner person
            return (person, tasks)

    runSql "array @> superset" $ runSelectReturningList $ select $ do
        m <- all_ (_meetings db)
        guard_ (meetingAttendees m `isSupersetOf_` array_ ["Marx"])
        return m

    -- runSql "window function average" $ runSelectReturningList $ select $ withWindow_
    --      (\p -> frame_ (partitionBy_ (personName p)) noOrder_ noBounds_)
    --     (\p w -> (p, avg_ (personAge p) `over_` w))
    --     (all_ (_people db))
