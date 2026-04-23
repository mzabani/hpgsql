module Database.PostgreSQL.Query.TH.SqlExp
  ( -- * QQ
    sqlExp,
  )
where

import qualified Hpgsql.Query
import Language.Haskell.TH.Quote

sqlExp :: QuasiQuoter
sqlExp = Hpgsql.Query.sql
