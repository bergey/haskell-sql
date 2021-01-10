{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main where

import qualified Database.PostgreSQL.Simple           as PGS
import qualified Database.PostgreSQL.Simple.FromField as PGS
import qualified Database.PostgreSQL.Simple.FromRow   as PGS

import qualified Hasql.Connection                     as HQL
import qualified Hasql.Decoders                       as HQL
import qualified Hasql.Decoders                       as D
import qualified Hasql.Encoders                       as Param
import qualified Hasql.Session                        as HQL
import qualified Hasql.Statement                      as HQL

import qualified PostgreSQL.Binary.Decoding           as PGB
import qualified Preql
import Preql.FromSql (deriveFromSql)
import qualified Preql.Effect                         as Preql
import qualified Preql.Wire.TypeInfo.Static           as OID

import           Control.DeepSeq                      (NFData (..))
import           Control.Exception                    (Exception, throwIO)
import           Control.Monad
import           Control.Monad.Trans.Reader           (ReaderT (..), ask,
                                                       runReaderT)
import           Criterion
import           Criterion.Main
import           Data.ByteString                      (ByteString)
import           Data.Functor.Contravariant           ((>$<))
import           Data.Int
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8, encodeUtf8)
import           Data.Vector                          (Vector)
import qualified Database.PostgreSQL.LibPQ            as PQ
import           GHC.Generics
import           System.Environment                   (lookupEnv)
import           System.Exit                          (ExitCode (..), exitWith)

main :: IO ()
main = do
  preql <- connectPreql =<< connectionString
  pgs <- PGS.connectPostgreSQL =<< connectionString
  hasql <- connectionString >>= HQL.acquire
      >>= \case
      Right c -> pure c
      Left e -> do
          putStrLn ("error connecting with hasql: " <> maybe "" show e)
          exitWith (ExitFailure 1)
  defaultMain
      [ bgroup "pg_type" $
        let
            typmod = -1 :: Int16
            isdefined = True
        in [ bench "preql" $ nfIO $ flip runReaderT preql $ do
                res <- Preql.query [Preql.sql| select typname, typnamespace, typowner, typlen, typbyval , typcategory, typispreferred, typisdefined, typdelim , typrelid, typelem, typarray from pg_type where typtypmod = ${typmod} and typisdefined = ${isdefined} |]
                return (res :: Vector TypeInfo)
           , bench "postgresql-simple" $ nfIO $ do
                 res <- PGS.query pgs "select typname, typnamespace, typowner, typlen, typbyval , typcategory, typispreferred, typisdefined, typdelim , typrelid, typelem, typarray from pg_type where typtypmod = ? and typisdefined = ?" (typmod, isdefined)
                 return (res :: [TypeInfo])
           , bench "hasql" $ nfIO $ do
                 let
                     statement = HQL.Statement "select typname, typnamespace, typowner, typlen, typbyval , typcategory, typispreferred, typisdefined, typdelim , typrelid, typelem, typarray from pg_type where typtypmod = $1 and typisdefined = $2"
                         ((fst >$< Param.param (Param.nonNullable Param.int2)) <>
                         (snd >$< Param.param (Param.nonNullable Param.bool)))
                         decodeTypeInfo
                         False
                 res <- HQL.run (HQL.statement (typmod, isdefined) statement) hasql
                     >>= either (error . show) return
                 return (res :: Vector TypeInfo)
         ]
      ]

connectPreql :: ByteString -> IO PQ.Connection
connectPreql string = do
    conn <- PQ.connectdb string
    status <- PQ.status conn
    unless (status == PQ.ConnectionOk) (error "bad connection")
    return conn

connectionString :: IO ByteString
connectionString = do
    m_dbname <- lookupEnv "HSQL_BENCHMARKS_DB"
    let dbname = case m_dbname of
            Just s  -> encodeUtf8 (T.pack s)
            Nothing -> "hsql_benchmarks"
    return $ "dbname=" <> dbname

instance NFData PQ.Oid where
    rnf (PQ.Oid oid) = rnf oid
deriving newtype instance NFData PgName

newtype PgName = PgName Text

data TypeInfo = TypeInfo
    { typname        :: !PgName
    , typnamespace   :: !PQ.Oid
    , typowner       :: !PQ.Oid
    , typlen         :: !Int16
    , typbyval       :: !Bool
    , typcategory    :: !Char
    , typispreferred :: !Bool
    , typisdefined   :: !Bool
    , typdelim       :: !Char
    , typrelid       :: !PQ.Oid
    , typelem        :: !PQ.Oid
    , typarray       :: !PQ.Oid
    }
    deriving (Generic, NFData)

instance Preql.FromSqlField PgName where
    fromSqlField = Preql.FieldDecoder (Preql.Oid OID.nameOid) (PgName <$> PGB.text_strict)
instance Preql.FromSql PgName where fromSql = Preql.notNull Preql.fromSqlField

instance PGS.FromField PgName where
    fromField field m_data =
        if PGS.typeOid field /= OID.nameOid
        then PGS.returnError PGS.Incompatible field ""
        else case m_data of
            Nothing -> PGS.returnError PGS.UnexpectedNull field ""
            Just bs -> return (PgName (decodeUtf8 bs))

instance PGS.FromRow TypeInfo where
    fromRow = TypeInfo <$>
        PGS.field <*> PGS.field <*> PGS.field <*> PGS.field <*>
        PGS.field <*> PGS.field <*> PGS.field <*> PGS.field <*>
        PGS.field <*> PGS.field <*> PGS.field <*> PGS.field

decodeTypeInfo :: HQL.Result (Vector TypeInfo)
decodeTypeInfo = D.rowVector $ TypeInfo
    <$> D.column (D.nonNullable (PgName <$> D.text))
    <*> decodeOid
    <*> decodeOid
    <*> D.column (D.nonNullable D.int2)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nonNullable D.char)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nonNullable D.char)
    <*> decodeOid
    <*> decodeOid
    <*> decodeOid

decodeOid :: HQL.Row PQ.Oid
decodeOid = D.column (D.nonNullable (PQ.Oid . fromIntegral <$> D.int8))

$(deriveFromSql ''TypeInfo)
