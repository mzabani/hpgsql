{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Hpgsql.LanguageHaskell.FromThExtension where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.LanguageExtensions.Type (Extension (..))
import qualified "template-haskell" Language.Haskell.TH as TH

fromThToGhcLibExtension :: TH.Extension -> Maybe Extension
fromThToGhcLibExtension = \case
  TH.AllowAmbiguousTypes -> Just AllowAmbiguousTypes
  TH.AlternativeLayoutRule -> Just AlternativeLayoutRule
  TH.AlternativeLayoutRuleTransitional -> Just AlternativeLayoutRuleTransitional
  TH.ApplicativeDo -> Just ApplicativeDo
  TH.Arrows -> Just Arrows
  TH.AutoDeriveTypeable -> Just AutoDeriveTypeable
  TH.BangPatterns -> Just BangPatterns
  TH.BinaryLiterals -> Just BinaryLiterals
  TH.BlockArguments -> Just BlockArguments
  TH.CApiFFI -> Just CApiFFI
  TH.CUSKs -> Just CUSKs
  TH.ConstrainedClassMethods -> Just ConstrainedClassMethods
  TH.ConstraintKinds -> Just ConstraintKinds
  TH.Cpp -> Just Cpp
  TH.DataKinds -> Just DataKinds
  TH.DatatypeContexts -> Just DatatypeContexts
  TH.DeepSubsumption -> Just DeepSubsumption
  TH.DefaultSignatures -> Just DefaultSignatures
  TH.DeriveAnyClass -> Just DeriveAnyClass
  TH.DeriveDataTypeable -> Just DeriveDataTypeable
  TH.DeriveFoldable -> Just DeriveFoldable
  TH.DeriveFunctor -> Just DeriveFunctor
  TH.DeriveGeneric -> Just DeriveGeneric
  TH.DeriveLift -> Just DeriveLift
  TH.DeriveTraversable -> Just DeriveTraversable
  TH.DerivingStrategies -> Just DerivingStrategies
  TH.DerivingVia -> Just DerivingVia
  TH.DisambiguateRecordFields -> Just DisambiguateRecordFields
  TH.DoAndIfThenElse -> Just DoAndIfThenElse
  TH.DuplicateRecordFields -> Just DuplicateRecordFields
  TH.EmptyCase -> Just EmptyCase
  TH.EmptyDataDecls -> Just EmptyDataDecls
  TH.EmptyDataDeriving -> Just EmptyDataDeriving
  TH.ExistentialQuantification -> Just ExistentialQuantification
  TH.ExplicitForAll -> Just ExplicitForAll
  TH.ExplicitNamespaces -> Just ExplicitNamespaces
  TH.ExtendedDefaultRules -> Just ExtendedDefaultRules
  TH.FieldSelectors -> Just FieldSelectors
  TH.FlexibleContexts -> Just FlexibleContexts
  TH.FlexibleInstances -> Just FlexibleInstances
  TH.ForeignFunctionInterface -> Just ForeignFunctionInterface
  TH.FunctionalDependencies -> Just FunctionalDependencies
  TH.GADTSyntax -> Just GADTSyntax
  TH.GADTs -> Just GADTs
  TH.GHCForeignImportPrim -> Just GHCForeignImportPrim
  TH.GeneralizedNewtypeDeriving -> Just GeneralizedNewtypeDeriving
  TH.HexFloatLiterals -> Just HexFloatLiterals
  TH.ImplicitParams -> Just ImplicitParams
  TH.ImplicitPrelude -> Just ImplicitPrelude
  TH.ImportQualifiedPost -> Just ImportQualifiedPost
  TH.ImpredicativeTypes -> Just ImpredicativeTypes
  TH.IncoherentInstances -> Just IncoherentInstances
  TH.InstanceSigs -> Just InstanceSigs
  TH.InterruptibleFFI -> Just InterruptibleFFI
  TH.JavaScriptFFI -> Just JavaScriptFFI
  TH.KindSignatures -> Just KindSignatures
  TH.LambdaCase -> Just LambdaCase
  TH.LexicalNegation -> Just LexicalNegation
  TH.LiberalTypeSynonyms -> Just LiberalTypeSynonyms
  TH.LinearTypes -> Just LinearTypes
  TH.MagicHash -> Just MagicHash
  TH.MonadComprehensions -> Just MonadComprehensions
  TH.MonoLocalBinds -> Just MonoLocalBinds
  TH.MonomorphismRestriction -> Just MonomorphismRestriction
  TH.MultiParamTypeClasses -> Just MultiParamTypeClasses
  TH.MultiWayIf -> Just MultiWayIf
  TH.NPlusKPatterns -> Just NPlusKPatterns
  TH.NamedFieldPuns -> Just NamedFieldPuns
  TH.NamedWildCards -> Just NamedWildCards
  TH.NegativeLiterals -> Just NegativeLiterals
  TH.NondecreasingIndentation -> Just NondecreasingIndentation
  TH.NullaryTypeClasses -> Just NullaryTypeClasses
  TH.NumDecimals -> Just NumDecimals
  TH.NumericUnderscores -> Just NumericUnderscores
  TH.OverlappingInstances -> Just OverlappingInstances
  TH.OverloadedLabels -> Just OverloadedLabels
  TH.OverloadedLists -> Just OverloadedLists
  TH.OverloadedRecordDot -> Just OverloadedRecordDot
  TH.OverloadedRecordUpdate -> Just OverloadedRecordUpdate
  TH.OverloadedStrings -> Just OverloadedStrings
  TH.PackageImports -> Just PackageImports
  TH.ParallelArrays -> Just ParallelArrays
  TH.ParallelListComp -> Just ParallelListComp
  TH.PartialTypeSignatures -> Just PartialTypeSignatures
  TH.PatternGuards -> Just PatternGuards
  TH.PatternSynonyms -> Just PatternSynonyms
  TH.PolyKinds -> Just PolyKinds
  TH.PostfixOperators -> Just PostfixOperators
  TH.QualifiedDo -> Just QualifiedDo
  TH.QuantifiedConstraints -> Just QuantifiedConstraints
  TH.QuasiQuotes -> Just QuasiQuotes
  TH.RankNTypes -> Just RankNTypes
  TH.RebindableSyntax -> Just RebindableSyntax
  TH.RecordWildCards -> Just RecordWildCards
  TH.RecursiveDo -> Just RecursiveDo
  TH.RelaxedLayout -> Just RelaxedLayout
  TH.RelaxedPolyRec -> Just RelaxedPolyRec
  TH.RoleAnnotations -> Just RoleAnnotations
  TH.ScopedTypeVariables -> Just ScopedTypeVariables
  TH.StandaloneDeriving -> Just StandaloneDeriving
  TH.StandaloneKindSignatures -> Just StandaloneKindSignatures
  TH.StarIsType -> Just StarIsType
  TH.StaticPointers -> Just StaticPointers
  TH.Strict -> Just Strict
  TH.StrictData -> Just StrictData
  TH.TemplateHaskell -> Just TemplateHaskell
  TH.TemplateHaskellQuotes -> Just TemplateHaskellQuotes
  TH.TraditionalRecordSyntax -> Just TraditionalRecordSyntax
  TH.TransformListComp -> Just TransformListComp
  TH.TupleSections -> Just TupleSections
  TH.TypeApplications -> Just TypeApplications
  TH.TypeData -> Just TypeData
  TH.TypeFamilies -> Just TypeFamilies
  TH.TypeFamilyDependencies -> Just TypeFamilyDependencies
  TH.TypeInType -> Just TypeInType
  TH.TypeOperators -> Just TypeOperators
  TH.TypeSynonymInstances -> Just TypeSynonymInstances
  TH.UnboxedSums -> Just UnboxedSums
  TH.UnboxedTuples -> Just UnboxedTuples
  TH.UndecidableInstances -> Just UndecidableInstances
  TH.UndecidableSuperClasses -> Just UndecidableSuperClasses
  TH.UnicodeSyntax -> Just UnicodeSyntax
  TH.UnliftedDatatypes -> Just UnliftedDatatypes
  TH.UnliftedFFITypes -> Just UnliftedFFITypes
  TH.UnliftedNewtypes -> Just UnliftedNewtypes
  TH.ViewPatterns -> Just ViewPatterns
#if MIN_VERSION_template_haskell(2,21,0)
  TH.ExtendedLiterals -> Just ExtendedLiterals
  TH.TypeAbstractions -> Just TypeAbstractions
#endif
#if MIN_VERSION_template_haskell(2,22,0)
  TH.ListTuplePuns -> Just ListTuplePuns
  TH.RequiredTypeArguments -> Just RequiredTypeArguments
#endif
#if MIN_VERSION_template_haskell(2,23,0)
  TH.MultilineStrings -> Just MultilineStrings
  TH.NamedDefaults -> Just NamedDefaults
  TH.OrPatterns -> Just OrPatterns
#endif
  -- Why a catch-all here after going through all the work of listing
  -- extensions above? Because of two conflicting goals:
  -- 1 - Not allocate and parse strings, plus run a Map search during compilation (see algo below)
  -- 2 - Support users compiling hpgsql with newer GHC versions
  --
  -- Goal 1 is arguably excessive over-refinement, and goal 2 is arguably
  -- pointless since it seems like (from my extremely limited experience)
  -- template-haskell and ghc-lib-parser will change with new releases
  -- anyway, but not being the annoying library that fails to compile or run
  -- with some user trying out a new GHC (after bumping version bounds themselves)
  -- feels important.
  -- So we achieve a little bit of both goals like this. This is also the reason
  -- why we have -Wno-overlapping-patterns in this file.
{- FOURMOLU_DISABLE -}
  someNewThExtension -> Map.lookup (show someNewThExtension) allGhcLibParserExtensions
{- FOURMOLU_ENABLE -}

-- | This Map is only useful by assuming the `Show` representations of language extensions in both
-- ghc-lib-parser and template-haskell match. That feels like a reasonable assumption.
allGhcLibParserExtensions :: Map String Extension
allGhcLibParserExtensions = Map.fromList $ map (\ex -> (show ex, ex)) [minBound .. maxBound]
