{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Data.Time (UTCTime)
import           Database.PostgreSQL.Simple
import           Data.UUID (UUID)

import qualified Data.Aeson as JSON
import qualified Data.Vector as V

main :: IO ()
main = do
    conn <- connectPostgreSQL "host=localhost port=5432 user=postgres"
    [Only i] <- query_ conn "select 2 + 2"
    print @Int i
    people <- query_ conn "select * from people"
    print @[(Int, String, Int, Double)] people
    [marx] <- query conn "select name, age from people where id = ?" (Only (2::Int))
    print @(String, Int) marx

    tasks <- query_ conn "select name, description from people join tasks on owner = people.id"
    print @[(String, String)] tasks

    meetings <- query_ conn "select id, time, details, attendees from meetings"
    print @[(UUID, UTCTime, JSON.Value, V.Vector String)] meetings
