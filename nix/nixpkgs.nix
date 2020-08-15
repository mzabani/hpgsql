{ system ? builtins.currentSystem }:
let
    ourOwnHaskellPkgsOverlay = final: prev: 
      let newHaskellPackages = prev.haskellPackages.extend (prev.haskell.lib.compose.packageSourceOverrides {
                                    hpgsql = ../hpgsql;
                                    hpgsql-benchmarks = ../hpgsql-benchmarks;
                                    hpgsql-simple-compat = ../hpgsql-simple-compat;
                              });
      in {
       haskellPackages = newHaskellPackages // {
        hpgsql = final.haskell.lib.dontCheck newHaskellPackages.hpgsql;
        hpgsql-benchmarks = final.haskell.lib.dontCheck newHaskellPackages.hpgsql-benchmarks; 
        hpgsql-simple-compat = final.haskell.lib.dontCheck newHaskellPackages.hpgsql-simple-compat;
      };
    };

    notReallyBrokenPkgsOverlay = final: prev:
      {
       haskellPackages = prev.haskellPackages.override {
         overrides = hsSelf: hsSuper: {
            streaming-postgresql-simple = final.haskell.lib.doJailbreak (final.haskell.lib.markUnbroken hsSuper.streaming-postgresql-simple);
          };
      };
    };

    # Profiling overlays taken from https://ryantm.github.io/nixpkgs/languages-frameworks/haskell/,
    # with package exclude-list from myself after build errors
    profilingPkgExcludeList = [ "th-abstraction" "inspection-testing" "th-lift" "wherefrom-compat" "th-compat" "th-expand-syns" "th-orphans" "haskell-src-meta" ];
    ghcName = "ghc9103";
    enableProfiling = true;
    enableProfilingOverlays = [
      # The first overlay modifies the GHC derivation so that it does or does not
      # build profiling versions of the core libraries bundled with it. It is
      # recommended to only use such an overlay if you are enabling profiling on a
      # platform that doesn't by default, because compiling GHC from scratch is
      # quite expensive.
      (final: prev:
      let
        inherit (final) lib;
      in

      {
        haskell = lib.recursiveUpdate prev.haskell {
          compiler.${ghcName} = prev.haskell.compiler.${ghcName}.override {
            # Unfortunately, the GHC setting is named differently for historical reasons
            enableProfiledLibs = enableProfiling;
          };
        };
      })

      (final: prev:
      let
        inherit (final) lib;
        haskellLib = final.haskell.lib.compose;
      in

      {
        haskell = lib.recursiveUpdate prev.haskell {
          packages.${ghcName} = prev.haskell.packages.${ghcName}.override {
            overrides = hfinal: hprev: {
              mkDerivation = args: hprev.mkDerivation (args //
                (if builtins.elem args.pname profilingPkgExcludeList then {} else
                {
                # Since we are forcing our ideas upon mkDerivation, this change will
                # affect every package in the package set.
                enableLibraryProfiling = enableProfiling;

                # To actually use profiling on an executable, executable profiling
                # needs to be enabled for the executable you want to profile. You
                # can either do this globally or…
                enableExecutableProfiling = enableProfiling;
              }));

              # …only for the package that contains an executable you want to profile.
              # That saves on unnecessary rebuilds for packages that you only depend
              # on for their library, but also contain executables (e.g. pandoc).
              # my-executable = haskellLib.enableExecutableProfiling hprev.my-executable;

              # If you are disabling profiling to save on build time, but want to
              # retain the ability to substitute from the binary cache. Drop the
              # override for mkDerivation above and instead have an override like
              # this for the specific packages you are building locally and want
              # to make cheaper to build.
              # my-library = haskellLib.disableLibraryProfiling hprev.my-library;
            };
          };
        };
      })

      # Some packages fail to build tests or whatnot with profiling enabled
      # (final: prev: 
      #   {
      #    haskellPackages = prev.haskellPackages // {
      #     th-abstraction = final.haskell.lib.dontCheck (prev.haskellPackages.th-abstraction.overrideAttrs (_: {
      #       # Building the test target fails for whatever reason
      #       enableLibraryProfiling = false;

      #       # To actually use profiling on an executable, executable profiling
      #       # needs to be enabled for the executable you want to profile. You
      #       # can either do this globally or…
      #       enableExecutableProfiling = false;
      #     }));
      #   };
      # })
    ];
in
    import (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/22fa6f7b5510a5492e46232efcb0a07f68d8be03.tar.gz";
      sha256 = "sha256:1md1mh2h6xz9cd80lfjnwrjyi575py02s8dm2naks6wd6n3ay3rr";
    }) {
      inherit system;
      overlays = [ notReallyBrokenPkgsOverlay ourOwnHaskellPkgsOverlay ]
        # ++ enableProfilingOverlays
        ;
    }
