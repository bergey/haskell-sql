{-# LANGUAGE ScopedTypeVariables #-}
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
import           Orphans

import           Control.Arrow ((&&&))
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader (ReaderT)
import           Data.Bifunctor
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Data.UUID (UUID)
import           Web.PathPieces

import qualified Data.ByteString.Char8 as B8
import qualified Data.UUID as UUID
import qualified Data.Aeson as JSON
import qualified Data.Vector as V

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person sql=people
    name Text
    age Int
    height Double sql=height_inches
    deriving Show

Task sql=tasks
    owner PersonId
    description Text
    deriving Show

Meeting sql=meetings
    Id UUID sqltype=uuid
    time UTCTime
    details Jsonb
    attendees [Text]
    deriving Show
|]

runSql :: (MonadUnliftIO m, IsPersistBackend backend,
           BaseBackend backend ~ SqlBackend) =>
          backend -> ReaderT backend m a -> m a
runSql = flip runSqlConn

main :: IO ()
main = runStderrLoggingT $
    withPostgresqlConn @_ @SqlBackend "host=localhost port=5432 user=postgres" $ \conn -> do
    people <- runSqlConn (selectList [] []) conn
    liftIO $ print @[Entity Person] people

    m_marx <- runSqlConn (get (PersonKey 2)) conn  -- full row
    liftIO $ print (fmap (personName &&& personAge) m_marx)

    -- No joins in persistent
    (tasks :: [(Single Text, Single Text)]) <- runSql conn $ rawSql "select p.name, t.description FROM people AS p JOIN tasks AS t ON t.owner = p.id" []
    liftIO $ print @[(Text, Text)] (bimap unSingle unSingle <$> tasks)

    -- meetings <- query_ conn "select id, time, details, attendees from meetings"
    meetings <- runSqlConn (selectList [] []) conn
    liftIO $ print @[Entity Meeting] meetings
