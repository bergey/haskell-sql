{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Database.PostgreSQL.Simple

main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost port=5432 user=postgres"
  [Only i] <- query_ conn "select 2 + 2"
  print @Int i
