{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
-- | Codegen for custom enum types, two ways.

module Enums where

import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics
import           Prelude
import           Text.Read (readMaybe)

-- import qualified Data.Vector as V
import qualified Data.UUID as UUID
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC

-- | = Generics

-- fromFieldEnum :: Generic a => Field -> Maybe ByteString -> Conversion a

class ToFieldEnumG rep where
    toFieldEnumG :: rep p -> Action

instance Constructor meta => ToFieldEnumG (C1 meta U1) where
    toFieldEnumG c = Escape (BSC.pack (conName c))

instance (ToFieldEnumG f, ToFieldEnumG g) => ToFieldEnumG (f :+: g) where
    toFieldEnumG (L1 f) = toFieldEnumG f
    toFieldEnumG (R1 g) = toFieldEnumG g

instance ToFieldEnumG c => ToFieldEnumG (D1 meta c) where
    toFieldEnumG (M1 c) = toFieldEnumG c

toFieldEnum :: (ToFieldEnumG (Rep a), Generic a) => a -> Action
toFieldEnum = toFieldEnumG . from

toFieldShow :: Show a => a -> Action
toFieldShow = Escape . BSC.pack . show

sqlFieldValueSyntax :: ToField a => a -> PgValueSyntax
sqlFieldValueSyntax = PgValueSyntax . pgBuildAction . pure . toField

-- | First argument is SQL type name
fromFieldEnum :: (Typeable a, Read a) => BS.ByteString -> Field -> Maybe BS.ByteString -> Conversion a
fromFieldEnum name field m_val = do
    actual <- typename field
    if actual == name then
        case m_val of
            Nothing -> returnError UnexpectedNull field ""
            Just v -> case readMaybe (BSC.unpack v) of
                Just a -> pure a
                Nothing -> returnError ConversionFailed field (ellipsis v)
        else returnError Incompatible field "expected meeting_type"

ellipsis :: BS.ByteString -> String
ellipsis x
    | BS.length x > 25 = take 20 (BSC.unpack x) ++ "[...]"
    | otherwise       = BSC.unpack x
