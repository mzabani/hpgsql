{
    system ? builtins.currentSystem
  , threading ? builtins.getEnv "HPGSQL_THREADED"
  , ghc ? "ghc9103"
  , pkgs ? import ./nix/nixpkgs.nix { inherit system threading; }
}:
let
  addPgExtensions = postgres: postgres.withPackages (ps: [ ps.pg_cron ]);
  haskellPackages = builtins.getAttr ghc pkgs.haskell.packages;
in
rec {
  hpgsql = haskellPackages.hpgsql;
  hpgsql-tests = haskellPackages.hpgsql-tests;
  # Building for Windows doesn't work yet. It would be nice to test with wine.
  hpgsql-tests-for-windows = (builtins.getAttr ghc pkgs.pkgsCross.x86_64-windows.haskell.packages).hpgsql-tests;
  hpgsql-simple-compat = haskellPackages.hpgsql-simple-compat;
  hpgsql-simple-compat-tests = haskellPackages.hpgsql-simple-compat-tests;
  hpgsql-benchmarks = haskellPackages.hpgsql-benchmarks;
  inherit haskellPackages;

  testsPg18 = { hspecArgs ? ""}: import ./nix/run-db-tests.nix { inherit pkgs hpgsql-tests hspecArgs; postgres = addPgExtensions pkgs.postgresql_18; };
  testsPg17 = { hspecArgs ? ""}: import ./nix/run-db-tests.nix { inherit pkgs hpgsql-tests hspecArgs; postgres = addPgExtensions pkgs.postgresql_17; };
  testsPg16 = { hspecArgs ? ""}: import ./nix/run-db-tests.nix { inherit pkgs hpgsql-tests hspecArgs; postgres = addPgExtensions pkgs.postgresql_16; };
  testsPg15 = { hspecArgs ? ""}: import ./nix/run-db-tests.nix { inherit pkgs hpgsql-tests hspecArgs; postgres = addPgExtensions pkgs.postgresql_15; };
  testsPg14 = { hspecArgs ? ""}: import ./nix/run-db-tests.nix { inherit pkgs hpgsql-tests hspecArgs; postgres = addPgExtensions pkgs.postgresql_14; };

  # Shells with specific-versioned postgres servers to run tests locally
  shellPg18 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_18; };
  shellPg17 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_17; };
  shellPg16 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_16; };
  shellPg15 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_15; };
  shellPg14 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_14; };

  shellForCITests = import ./nix/test-shell-ci.nix { inherit pkgs; };
}
  
