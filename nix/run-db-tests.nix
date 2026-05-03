{ postgres, pkgs, hpgsql-tests, hspecArgs }:
let fs = pkgs.lib.fileset;
in
 pkgs.stdenv.mkDerivation {
     name = "hpgsql-tests-with-db-results";
     src = fs.toSource {
      root = ../.;
      fileset = fs.unions [ ../conf/test-db ../scripts/init-pg-cluster.sh ../scripts/wait-for-pg-ready.sh ];
     };
     nativeBuildInputs = [ postgres pkgs.bash pkgs.coreutils pkgs.glibcLocales ];
     installPhase = ''
      patchShebangs scripts/*.sh
      mkdir "$out"
      mkdir -p local/temp-pg-data
      export LANG=en_US.UTF-8
      export PGDATA="local/temp-pg-data"
      export PGDATABASE="postgres"
      export PGPORT="5434"
      export PGHOST="/tmp"
      export PGUSER="postgres"
      scripts/init-pg-cluster.sh ./conf/test-db
      trap "pg_ctl stop || true" EXIT ERR
      pg_ctl -l "$out/pg_ctl_init.log" start
      scripts/wait-for-pg-ready.sh
      # Create things needed by tests
      psql -c "CREATE EXTENSION citext; CREATE USER user_pass WITH PASSWORD 'hpgsql-password'; SET password_encryption = 'md5'; CREATE USER user_md5 WITH PASSWORD 'hpgsql-password'"

      ${hpgsql-tests}/bin/hpgsql-tests ${hspecArgs}

      pg_ctl stop
      trap - EXIT ERR
    '';
  }
