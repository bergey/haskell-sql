{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Data.UUID (UUID)

import qualified Data.Aeson as JSON
import qualified Data.Vector as V

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person sql=people
    name Text
    age Int
    height Double sql=height_inches
    deriving Show
|]

main :: IO ()
main = runStderrLoggingT $
    withPostgresqlConn @_ @SqlBackend "host=localhost port=5432 user=postgres" $ \conn -> do
    people <- runSqlConn (selectList [] []) conn
    liftIO $ print @[Entity Person] people

    -- marx <- get (PersonKey 2)  -- full row
    -- print (personName marx, personAge marx)

    -- tasks <- query_ conn "select name, description from people join tasks on owner = people.id"
    -- print @[(String, String)] tasks

    -- meetings <- query_ conn "select id, time, details, attendees from meetings"
    -- print @[(UUID, UTCTime, JSON.Value, V.Vector String)] meetings
