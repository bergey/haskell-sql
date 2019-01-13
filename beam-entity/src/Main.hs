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

import Entity

import           Data.Aeson as JSON
import           Data.Coerce
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Data.Time
import           Data.UUID (UUID)
import           Data.Vector (Vector)
import           Database.Beam
-- import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Postgres
-- import           Database.PostgreSQL.Simple.FromField
import           Prelude
-- import           Text.Read as X (readMaybe)

-- import qualified Data.Vector as V
import qualified Data.UUID as UUID

mkID :: Coercible UUID.UUID id => String -> id
mkID = coerce . fromJust . UUID.fromString

data PersonT f = Person
    { personName :: Columnar f Text
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
    data PrimaryKey PersonT f = PersonPartial deriving Generic
    primaryKey = const PersonPartial
instance Beamable (PrimaryKey PersonT)

data TodoDb f = TodoDB
    { _people :: f (TableEntity (EntityT PersonT))
    } deriving Generic
instance Database be TodoDb

db :: DatabaseSettings be TodoDb
db = defaultDbSettings `withDbModification`
    dbModification {
        _people = modifyTable id $ tableModification
            { entityId = "id"
            , entityVal = tableModification
                { personName = "name"
                }
            }
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

    runSql "insert full" $ runInsert $ insert (_people db) $
        insertValues
            [ Entity 102 (Person "Jones" 6 11)
            , Entity 103 (Person "', 1, 2, 3, '" 0 99)
            ]

    runSql "insert with default PK" $ runInsert $ insert (_people db) $
        insertExpressions
            [ Entity default_ (val_ (Person "Proudhon" (-1) (-1)))
            , Entity default_ (val_ (Person "Kropotkin" 180 64))
            ]
