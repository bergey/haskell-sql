{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Data.Int
import           Hasql.Connection
import           Hasql.Session
import           Hasql.Statement

import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

main :: IO ()
main = do
  Right conn <- acquire "host=localhost port=5432 user=postgres"
  e_i <- run (statement () (Statement "select 2 + 2" HE.unit (HD.singleRow (HD.column HD.int4)) False)) conn
  print @(Either QueryError Int32) e_i
  e_people <- run (statement () (Statement "select * from people" HE.unit
                                (HD.rowList ((,,,) <$> HD.column HD.int4 <*> HD.column HD.text <*> HD.column HD.int4 <*> HD.column HD.float8)) False)) conn
  print e_people
  e_marx <- run (statement 2 (Statement "select name, age from people where id = $1" (HE.param HE.int4) (HD.singleRow ((,) <$> HD.column HD.text <*> HD.column HD.int4)) False)) conn
  print e_marx

  e_tasks <- run (statement () (Statement "select name, description from people join tasks on owner = people.id" (HE.unit) (HD.rowList ((,) <$> HD.column HD.text <*> HD.column HD.text)) False)) conn
  print e_tasks
