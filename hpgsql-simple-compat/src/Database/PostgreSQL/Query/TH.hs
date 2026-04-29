module Database.PostgreSQL.Query.TH
  ( -- * Deriving instances

    -- deriveEverything,
    -- , module Database.PostgreSQL.Query.TH.Entity
    -- , module Database.PostgreSQL.Query.TH.Enum
    -- , module Database.PostgreSQL.Query.TH.Row
    module Database.PostgreSQL.Query.TH.SqlExp,
  )
where

-- import Database.PostgreSQL.Query.TH.Entity
-- import Database.PostgreSQL.Query.TH.Enum
-- import Database.PostgreSQL.Query.TH.Row
import Database.PostgreSQL.Query.TH.SqlExp

-- | Calls sequently `deriveFromRow` `deriveToRow` `deriveEntity`. E.g. code like this:
--
-- @
-- data Agent = Agent
--    { aName          :: !Text
--    , aAttributes    :: !HStoreMap
--    , aLongWeirdName :: !Int
--    } deriving (Ord, Eq, Show)
--
-- \$(deriveEverything
--  def { eoIdType        = ''Id
--      , eoTableName     = textFN . toUnderscore'
--      , eoColumnNames   = textFN . toUnderscore' . drop 1
--      , eoDeriveClasses =
--        [''Show, ''Read, ''Ord, ''Eq
--        , ''FromField, ''ToField, ''PathPiece]
--      }
--  ''Agent )
-- @
--
-- will generate that:
--
-- @
-- instance ToRow Agent where
--    toRow (Agent a_aE3w a_aE3x a_aE3y)
--        = [toField a_aE3w, toField a_aE3x, toField a_aE3y]
-- instance FromRow Agent where
--  fromRow
--      = Agent <$> Database.PostgreSQL.Simple.FromRow.field
--        <*> Database.PostgreSQL.Simple.FromRow.field
--        <*> Database.PostgreSQL.Simple.FromRow.field
-- instance Database.PostgreSQL.Query.Entity Agent where
--    newtype EntityId Agent
--        = AgentId {getAgentId :: Id}
--        deriving (Show, Read, Ord, Eq, FromField, ToField, PathPiece)
--    tableName _ = "agent"
--    fieldNames _ = ["name", "attributes", "long_weird_name"]
-- type AgentId = EntityId Agent
-- @
-- deriveEverything :: EntityOptions -> Name -> Q [Dec]
-- deriveEverything opts tname =
--   fmap concat $
--     sequence
--       [ deriveToRow tname,
--         deriveFromRow tname,
--         deriveEntity opts tname
--       ]
