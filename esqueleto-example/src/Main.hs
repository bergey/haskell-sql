{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Database.Esqueleto
import           Database.Persist (Entity(..), Key(..))
import           Database.Persist.Postgresql hiding ((==.))
import           Database.Persist.TH
import           Orphans

import           Control.Arrow ((&&&))
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader (ReaderT)
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
    people <- runSql conn $ select $ from $ \person -> return person
    liftIO $ print @[Entity Person] people

    m_marx <- runSql conn $ select $ from $ \person -> do
        where_ ( person ^. PersonId ==. val (PersonKey 2) )
        return (person ^. PersonName, person ^. PersonAge)
    liftIO $ print m_marx

    tasks <- runSql conn $ select $ from $ \(person `InnerJoin` task) -> do
        on (task ^. TaskOwner ==. person ^. PersonId)
        return (person ^. PersonName, task ^. TaskDescription)
    liftIO $ print tasks

    -- meetings <- query_ conn "select id, time, details, attendees from meetings"
    meetings <- runSql conn $ select $ from $ \meeting -> return meeting
    liftIO $ print @[Entity Meeting] meetings
