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
import           Prelude hiding (id, head, tail, init, last, read, until, print)

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

import qualified Prelude
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
    details Jsonb default='{}'
    attendees [Text] default='{}' sqltype=text[]
    deriving Show
|]
-- When `default` or `sqltype` are given explicitly as above,
-- Persistent includes them in every migration.  It doesn't check if
-- the DB matches, or that the change has already been applied.  I
-- expect that means migrations get quite hard to read as the number
-- of these grows.

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print

runSql :: (MonadUnliftIO m, IsPersistBackend backend,
           BaseBackend backend ~ SqlBackend) =>
          backend -> ReaderT backend m a -> m a
runSql = flip runSqlConn

main :: IO ()
main = runStderrLoggingT $
    withPostgresqlConn @_ @SqlBackend "host=localhost port=5432 user=postgres" $ \conn -> do
    runSql conn $ printMigration migrateAll
    people <- runSqlConn (selectList [] []) conn
    print @[Entity Person] people

    m_marx <- runSqlConn (get (PersonKey 2)) conn  -- full row
    print (fmap (personName &&& personAge) m_marx)

    -- No joins in persistent
    (tasks :: [(Single Text, Single Text)]) <- runSql conn $ rawSql "select p.name, t.description FROM people AS p JOIN tasks AS t ON t.owner = p.id" []
    print @[(Text, Text)] (bimap unSingle unSingle <$> tasks)

    -- params with ?, ?? will name all the columns in the expected type
    -- but ?? won't work if we have an alias for `tasks`
    let daniel = "Daniel" :: Text
    myTasks <- runSql conn $ rawSql "select ?? FROM tasks JOIN people AS p ON tasks.owner = p.id WHERE p.name = ?" [toPersistValue daniel]
    print @[Entity Task] myTasks

    -- meetings <- query_ conn "select id, time, details, attendees from meetings"
    meetings <- runSqlConn (selectList [] []) conn
    print @[Entity Meeting] meetings

    smallOrOld <- runSql conn $ selectList ([PersonAge >. 22] ||. [PersonHeight <. 33]) []
    print smallOrOld

    sorted <- runSql conn $ selectList [] [Desc MeetingTime]
    print sorted

    (countTasks :: [(Entity Person, Single Int)]) <- runSql conn $ rawSql "select ??, count(*) FROM people JOIN tasks ON tasks.owner = people.id GROUP BY people.id, name, age, height_inches" []
    print (second unSingle <$> countTasks)

    -- The SQL below is syntactically invalid; the error is not caught until it runs
    -- (tasks :: [(Single Text, Single Text)]) <- runSql conn $ rawSql "select p.name, t.description" []
    return ()
