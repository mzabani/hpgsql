{ pkgs, postgres }:
pkgs.mkShell {
  buildInputs = [ postgres pkgs.coreutils pkgs.bash pkgs.glibcLocales pkgs.run ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.strace ];
  description = "Test shell with postgres available and initializing";
  shellHook = ''
    set -eo pipefail

    export PGDATA="$(mktemp -d)"
    export PGDATABASE="postgres"
    export PGPORT="5434"
    export PGHOST="/tmp"
    export PGUSER="postgres"
    trap "pg_ctl stop" EXIT ERR
    scripts/init-pg-cluster.sh ./conf/test-db
    pg_ctl start || {
      mkdir -p failed-test-db-logs
      cp "$PGDATA/log" failed-test-db-logs/ -R
      exit 1
    }
    scripts/wait-for-pg-ready.sh

    # Create things needed by tests
    psql -c "CREATE EXTENSION citext; CREATE USER user_pass WITH PASSWORD 'hpgsql-password'; SET password_encryption = 'md5'; CREATE USER user_md5 WITH PASSWORD 'hpgsql-password'"
  '';
}
