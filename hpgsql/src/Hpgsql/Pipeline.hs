-- |
-- Pipelines are a way to build a sequence of SQL statements that get sent in a single round-trip
-- to PostgreSQL. This helps avoid latencies.
--
-- Here's an example:
--
-- > f :: Int -> IO (Stream (Of Aeson.Value) IO ())
-- > f val = do
-- >   (updateTbl :: IO (), aggRes :: IO (Only Int), largeResults) <-
-- >     runPipeline conn $
-- >       (,,)
-- >         <$> pipelineCmd_ [sql|UPDATE tbl SET val=#{val}|]
-- >         <*> pipeline1 [sql|SELECT SUM(val) FROM tbl|]
-- >         <*> pipelineSWith
-- >           (rowDecoder @(Vector Int, Vector Text))
-- >           -- We use a prepared statement for the query below
-- >           [sqlPrep|SELECT x, y FROM tbl|]
-- >   updateTbl
-- >   Only total <- aggRes
-- >   Streaming.map Aeson.toJSON <$> largeResults
--
-- All the queries above are sent in a single round-trip and execute in order in the server.
--
-- For all pipeline usage, you must always:
--
-- * Run the returned `IO` values in order.
-- * Run the returned `IO` values in the same thread that called `runPipeline`. This also applies to consuming Streams.
--
-- Or else Hpgsql can deadlock or run into undefined behaviour.
module Hpgsql.Pipeline
  ( Pipeline, -- Don't export constructor
    runPipeline,
    pipelineS,
    pipelineSWith,
    pipelineSMWith,
    pipeline,
    pipelineWith,
    pipelineCmd,
    pipelineCmd_,
    pipeline1,
    pipeline1With,
  )
where

import Hpgsql.Internal (Pipeline, pipeline, pipeline1, pipeline1With, pipelineCmd, pipelineCmd_, pipelineS, pipelineSMWith, pipelineSWith, pipelineWith, runPipeline)
