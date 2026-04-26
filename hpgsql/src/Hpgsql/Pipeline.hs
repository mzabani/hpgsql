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
