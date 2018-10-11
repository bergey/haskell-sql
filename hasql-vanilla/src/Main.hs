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
