{ system ? builtins.currentSystem, pkgs ? import ./nix/nixpkgs.nix { inherit system; } }:
let
  addPgExtensions = postgres: postgres.withPackages (ps: [ ps.pg_cron ]);
in
{
  hpgsql = pkgs.haskellPackages.hpgsql;
  hpgsql-simple-compat = pkgs.haskellPackages.hpgsql-simple-compat;
  hpgsql-benchmarks = pkgs.haskellPackages.hpgsql-benchmarks;
  haskellPackages = pkgs.haskellPackages;

  # testsPg16 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs coddtests hspecArgs; postgres = addPgExtensions pkgs.postgresql_16; };
  # testsPg15 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs coddtests hspecArgs; postgres = addPgExtensions pkgs.postgresql_15; };
  # testsPg14 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs coddtests hspecArgs; postgres = addPgExtensions pkgs.postgresql_14; };
  # testsPg13 = { hspecArgs ? "--match /DbDependentSpecs/"}: import ./nix/run-db-tests.nix { inherit pkgs coddtests hspecArgs; postgres = addPgExtensions pkgs.postgresql_13; };
  # testsNoDb = { hspecArgs ? "--skip /DbDependentSpecs/ --skip /SystemResourcesSpecs/" }: import ./nix/run-no-db-tests.nix { inherit pkgs coddtests hspecArgs; };
  # testsSystemResources = import ./nix/run-system-resources-tests.nix { inherit pkgs coddtests; postgres = addPgExtensions pkgs.postgresql_16; };

  # Shells with specific-versioned postgres servers to run tests locally
  shellPg16 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_16; };
  shellPg15 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_15; };
  shellPg14 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_14; };
  shellPg13 = import ./nix/test-shell-pg.nix { inherit pkgs; postgres = addPgExtensions pkgs.postgresql_13; };

  shellForCITests = import ./nix/test-shell-ci.nix { inherit pkgs; };
}
  
