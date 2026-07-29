{-# OPTIONS_GHC -Wno-missing-fields #-}

module Hpgsql.GhcParserOpts (fakeSettings) where

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
