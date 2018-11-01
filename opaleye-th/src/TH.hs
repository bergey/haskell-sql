-- | What if we could make all the boilerplate from a single quasiquote?
-- The syntax below is exactly like `persistent`; just needs a pile of TH to drive it
-- [opaleye|
--     Person sql=personTable
--         name Text
--         age Int32
--         height Double sql=height_inches
-- |]

module TH where

import Data.Text (Text)
import Language.Haskell.TH

data ColumnMeta = ColumnMeta
    { haskellName :: Text
    , sqlName :: Text
    , haskellType :: Name
    }

data TableMeta = TableMeta
    { haskellName :: Text
    , sqlName :: Text
    , columns :: [ColumnMeta]
    }
