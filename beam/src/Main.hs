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
import           Data.UUID (UUID)
import           Database.Beam
import           Database.Beam.Postgres
import           Prelude

import qualified Data.UUID as UUID

mkID :: Coercible UUID.UUID id => String -> id
mkID = coerce . fromJust . UUID.fromString

data PersonT f = Person
    { _id :: Columnar f Int
    , _name :: Columnar f Text
    , _age :: Columnar f Int
    , _height_inches :: Columnar f Double
    } deriving Generic

type Person = PersonT Identity
type PersonId = PrimaryKey PersonT Identity

deriving instance Show Person
deriving instance Eq Person

instance Beamable PersonT
instance Table PersonT where
    data PrimaryKey PersonT f = PersonId (Columnar f Int) deriving Generic
    primaryKey = PersonId . _id
instance Beamable (PrimaryKey PersonT)

data TodoDb f = TodoDB
    { _people :: f (TableEntity PersonT)
    } deriving Generic
instance Database be TodoDb

todoDb :: DatabaseSettings be TodoDb
todoDb = defaultDbSettings

main :: IO ()
main = do
    conn <- connect (ConnectInfo "localhost" 5432 "postgres" "" "")
    runBeamPostgresDebug putStrLn conn $ do
        runInsert $ insert (_people todoDb) $ insertValues
            [ -- Person 3 "Jones" 6 11
             Person 4 "', 1, 2, 3, '" 0 99
            ]
    return ()
