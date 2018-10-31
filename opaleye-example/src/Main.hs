{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- import           Data.Profunctor.Product (p2, p3)
-- import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye

import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Data.UUID (UUID)
import           Database.PostgreSQL.Simple hiding (Query)

import qualified Data.Aeson as JSON
import qualified Data.Vector as V

-- personTable :: Table (Column SqlText, Column SqlInt4, Column SqlFloat8)
--                      (Column SqlText, Column SqlInt4, Column SqlFloat8)
-- personTable = table "personTable" (p3 ( tableColumn "name"
--                                       , tableColumn "age"
--                                       , tableColumn "height" ))

data Person' a b c = Person'
    { name :: a
    , age :: b
    , height :: c
    }
type Person = Person' Text Int Double
type PersonColumn = Person' (Column SqlText) (Column SqlInt4) (Column SqlFloat8)
$(makeAdaptorAndInstance "pPerson" ''Person')

personTable :: Table PersonColumn PersonColumn
personTable = table "personTable" (
    pPerson Person'
        { name = tableColumn "name"
        , age = tableColumn "age"
        , height = tableColumn "height_inches"
        })

-- What if we could make all the above from a single quasiquote?
-- The syntax below is exactly like `persistent`; just needs a pile of TH to drive it
-- [opaleye|
--     Person sql=personTable
--         name Text
--         age Int32
--         height Double sql=height_inches
-- |]

personQuery :: Query PersonColumn
personQuery = queryTable personTable

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
