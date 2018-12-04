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

import           Database.Esqueleto
import           Database.Persist (Entity(..), Key(..))
import           Database.Persist.Postgresql hiding ((==.), (>.), (<.), (||.), count)
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

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as B8
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Prelude

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

    smallOrOld <- runSql conn $ select $ from $ \person -> do
        where_ (person ^. PersonAge >. val 22 ||. person ^. PersonHeight <. val 33)
        return person
    print smallOrOld

    sorted <- runSql conn $ select $ from $ \meeting -> do
        orderBy [ desc (meeting ^. MeetingTime) ]
        return meeting
    print sorted

    -- select p.id, name, age, height_inches, count(*) FROM people AS p JOIN tasks ON tasks.owner = p.id GROUP BY p.id, name, age, height_inches;
    (countTasks :: [(Entity Person, Value Int)]) <- runSql conn $ select $ from $ \(person `InnerJoin` task) -> do
        on (person ^. PersonId ==. task ^. TaskOwner)
        groupBy (person ^. PersonId, person ^. PersonName, person ^. PersonAge, person ^. PersonHeight)
        return (person, countRows)
    print (second unValue <$> countTasks)
