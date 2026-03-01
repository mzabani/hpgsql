{ system ? builtins.currentSystem, pkgs ? import ./nix/nixpkgs.nix { inherit system; } }:
let
  addPgExtensions = postgres: postgres.withPackages (ps: [ ps.pg_cron ]);
in
rec {
  hpgsql = pkgs.haskellPackages.hpgsql;
  hpgsql-tests = pkgs.haskellPackages.hpgsql-tests;
  hpgsql-simple-compat = pkgs.haskellPackages.hpgsql-simple-compat;
  hpgsql-benchmarks = pkgs.haskellPackages.hpgsql-benchmarks;
  haskellPackages = pkgs.haskellPackages;

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
  
