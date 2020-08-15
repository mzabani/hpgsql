{ pkgs ? import ./nixpkgs.nix {} }:
let
  postgres = pkgs.postgresql_16.withPackages (ps: with ps; [ pg_cron ]);
in
# We add our packages to the haskell package set
  (pkgs.haskellPackages.extend (pkgs.haskell.lib.compose.packageSourceOverrides {
    hpgsql = ./.;
    hpgsql-simple-compat = ./lib/hpgsql-simple-compat;
  }))
  # We call on this set shellFor to drop us into a shell containing the dependencies of frontend and backend:
  .shellFor {
    packages = p: [p.hpgsql p.hpgsql-simple-compat];
    withHoogle = true;
    buildInputs = [ pkgs.cabal-install pkgs.run postgres ];
  }

