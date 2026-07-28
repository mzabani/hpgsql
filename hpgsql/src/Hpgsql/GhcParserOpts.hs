{-# OPTIONS_GHC -Wno-missing-fields #-}

module Hpgsql.GhcParserOpts (parserDynFlags) where

import GHC.Driver.Session (DynFlags, defaultDynFlags, xopt_set)
import GHC.LanguageExtensions.Type
import GHC.Platform (genericPlatform)
import GHC.Settings
import GHC.Settings.Config (cProjectVersion)
import GHC.Utils.Fingerprint (fingerprint0)

-- | Fake GHC 'Settings' with only the fields the parser needs.
-- All other fields are left undefined; this is why we suppress
-- the missing-fields warning for this module only.
fakeSettings :: Settings
fakeSettings =
  Settings
    { sGhcNameVersion = GhcNameVersion "ghc" cProjectVersion,
      sFileSettings = FileSettings {},
      sTargetPlatform = genericPlatform,
      sPlatformMisc = PlatformMisc {},
      sToolSettings = ToolSettings {toolSettings_opt_P_fingerprint = fingerprint0}
    }

parserDynFlags :: DynFlags
parserDynFlags =
  foldl
    xopt_set
    (defaultDynFlags fakeSettings)
    [ OverloadedStrings,
      OverloadedRecordDot,
      TupleSections,
      LambdaCase,
      MultiWayIf,
      PostfixOperators,
      QuasiQuotes,
      UnicodeSyntax,
      MagicHash,
      ForeignFunctionInterface,
      TemplateHaskell,
      RankNTypes,
      MultiParamTypeClasses,
      RecursiveDo,
      TypeApplications
    ]
