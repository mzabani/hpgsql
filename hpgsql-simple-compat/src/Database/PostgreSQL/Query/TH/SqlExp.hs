module Database.PostgreSQL.Query.TH.SqlExp
  ( -- * QQ
    sqlExp,
  )
where

import Hpgsql.Query (sql)
import Language.Haskell.TH.Quote (QuasiQuoter)

-- | Quasiquoter for SQL expressions. Returns a 'SqlBuilder' (which is an alias
-- for hpgsql's 'Hpgsql.Query.Query'). Supports @#{}@ for parameter interpolation
-- and @^{}@ for query fragment embedding.
sqlExp :: QuasiQuoter
sqlExp = sql
