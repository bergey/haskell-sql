{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Data.Int
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Data.UUID (UUID)
import           GHC.Generics (Generic)
import           Hasql.Connection
import           Hasql.Generic.HasParams (HasParams(..), HasEValue(..))
import           Hasql.Generic.HasRow (HasRow(..), HasDValue(..))
import           Hasql.Session
import           Hasql.Statement

import qualified Data.Aeson as JSON
import qualified Data.Vector as V
import qualified Generics.SOP as SOP
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

main :: IO ()
main = do
    Right conn <- acquire "host=localhost port=5432 user=postgres"
    e_i <- run (statement () (Statement "select 2 + 2" HE.unit (HD.singleRow (HD.column HD.int4)) False)) conn
    print @(Either QueryError Int32) e_i
    e_people <- run (statement () (Statement "select * from people" HE.unit
                                  (HD.rowList mkRow) False)) conn
    print @(Either _ [Person]) e_people
    e_marx <- run (statement 2 (Statement "select name, age from people where id = $1" (HE.param HE.int4) (HD.singleRow mkRow) False)) conn
    print @(Either _ (Text, Int32)) e_marx

    e_tasks <- run (statement () (Statement "select name, description from people join tasks on owner = people.id" (HE.unit) (HD.rowList mkRow) False)) conn
    print @(Either _ [(Text, Text)]) e_tasks

    e_meetings <- run (statement () (Statement "select id, time, details, attendees from meetings" HE.unit (HD.rowList mkRow) False)) conn
    print @(Either _ [Meeting]) e_meetings

data Person = Person
    { _id :: Maybe Int32
    , _name :: Text
    , _age :: Int32
    , _height :: Double
    } deriving (Show, Eq, Generic)

instance SOP.Generic Person
instance HasParams Person
instance HasRow Person

data Meeting = Meeting
    { meetingId :: UUID
    , meetingTime :: UTCTime
    , meetingDetails :: JSON.Value
    , meetingAttendees :: V.Vector Text
    } deriving (Show, Eq, Generic)

instance SOP.Generic Meeting
instance HasParams Meeting
instance HasRow Meeting
