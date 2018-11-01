{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
-- | What if we could make all the boilerplate from a single quasiquote?
-- The syntax below is exactly like `persistent`; just needs a pile of TH to drive it
-- [opaleye|
--     Person sql=personTable
--         name Text
--         age Int32
--         height Double sql=height_inches
-- |]

module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Traversable

import Opaleye (Column, SqlText, SqlInt4, SqlFloat8)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)

data ColumnMeta = ColumnMeta
    { cmHaskellName :: String
    , cmSqlName :: String
    , cmHaskellType :: TypeQ
    , cmSqlType :: TypeQ
    }

data TableMeta = TableMeta
    { haskellName :: String
    , sqlName :: String
    , columns :: [ColumnMeta]
    }

a1Names :: Int -> [String]
a1Names n = take n names where
  names = [c : "" | c <- ['a'..'z']] ++ [ c : show i | i <- [1..], c <- ['a'..'z'] ]

recordConstructor :: String -> Name -> VarBangType
recordConstructor field tyN =
    (mkName field, bang, VarT tyN)
    where bang = Bang NoSourceUnpackedness NoSourceStrictness

makePrimedType :: Name -> [ColumnMeta] -> Q Dec
makePrimedType name columns = do
    tyVars <- traverse newName (a1Names (length columns))
    let con = RecC name (zipWith recordConstructor (map cmHaskellName columns) tyVars)
    return $ DataD [] name (map PlainTV tyVars) Nothing [con] []

-- makeHaskellAlias :: String -> Name -> [ColumnMeta] -> Q Dec
-- makeHaskellAlias name primedName columns = do
--     tyVars <- for columns $ \(ColumnMeta _ _ ty)  -> do
--         (TyConI tyCon) <- reify ty
--         case tyCon of
--             DataD _ nm [] cs _ _  -> return (nm, tyVars, cs)
--             NewtypeD _ nm [] c _ -> return (nm, tyVars, [c])
--             TySynD _ [] ty -> return ty
--             _ -> fail "tyCon must be a Type of kind Type"
--     tySynD (mkName name) [] (foldl apply (conT primedName) tyVars)

makeHaskellAlias :: String -> Name -> [ColumnMeta] -> Q Dec
makeHaskellAlias name primedName columns =
    tySynD (mkName name) [] (foldl appT (conT primedName) (map cmHaskellType columns))

type family SqlType t
type instance SqlType String = SqlText
type instance SqlType Int = SqlInt4
type instance SqlType Double = SqlFloat8

makeTable :: TableMeta -> Q [Dec]
makeTable t = do
    let primedName = mkName (haskellName t ++ "'")
    primedType <- makePrimedType primedName (columns t)
    haskellTypeAlias <- makeHaskellAlias (haskellName t) primedName (columns t)
    sqlAlias <- tySynD (mkName (haskellName t ++ "Column")) [] (foldl appT (conT primedName) (map cmSqlType (columns t)))
    -- adaptor <- makeAdaptorAndInstance ('p' : haskellName t) primedName
    return $ [ primedType, haskellTypeAlias, sqlAlias] -- ++ adaptor

personMeta :: TableMeta
personMeta = TableMeta
    { haskellName = "Person"
    , sqlName = "people"
    , columns =
            [ ColumnMeta "name" "name" [t|String|] [t|Column SqlText|]
            , ColumnMeta "age" "age" [t|Int|] [t|Column SqlInt4|]
            , ColumnMeta "height" "height_inches" [t|Double|] [t|Column SqlFloat8|]
            ]
    }
