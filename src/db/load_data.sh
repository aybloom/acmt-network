#!/bin/bash
set -e

GISDATA="/gisdata"
YEAR="${GEOCODER_YEAR:-2025}"
STATES="${GEOCODER_STATES:-WA}"

export PGPASSWORD="$POSTGRES_PASSWORD"
export PGBIN="/usr/lib/postgresql/18/bin"
#export PGPORT="5432"
#export PGHOST="localhost"
export PGUSER="$POSTGRES_USER"
export PGDATABASE="$POSTGRES_DB"

PSQL="psql -U $POSTGRES_USER -d $POSTGRES_DB"

echo "DEBUG: PSQL=$PSQL"
echo "DEBUG: PGHOST=${PGHOST:-<not set>}"
echo "DEBUG: PGPORT=${PGPORT:-<not set>}"

mkdir -p "$GISDATA/temp"

echo "========================================"
echo "PostGIS TIGER geocoder setup"
echo "TIGER year: $YEAR"
echo "States: $STATES"
echo "========================================"

echo "Creating TIGER data schema..."

$PSQL -c "CREATE SCHEMA IF NOT EXISTS tiger_data;"

echo "Creating extensions..."

$PSQL -c "CREATE EXTENSION IF NOT EXISTS postgis;"
$PSQL -c "CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;"
$PSQL -c "CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder;"
$PSQL -c "CREATE EXTENSION IF NOT EXISTS address_standardizer;"

echo "Setting TIGER year..."

$PSQL -c "
UPDATE tiger.loader_variables
SET tiger_year = $YEAR;
"

echo "Generating national TIGER loader..."

$PSQL -At -c \
  "SELECT tiger.loader_generate_nation_script('sh');" \
  > "$GISDATA/nation_load.sh"

chmod +x "$GISDATA/nation_load.sh"

sed -i 's/localhost/\/var\/run\/postgresql/g' "$GISDATA/nation_load.sh"

# Use the container's PostgreSQL 18 connection settings.
sed -i \
  -e '/^export PGBIN=/d' \
  -e '/^export PGUSER=/d' \
  -e '/^export PGPASSWORD=/d' \
  -e '/^export PGDATABASE=/d' \
  "$GISDATA/nation_load.sh"

echo "Running national TIGER loader..."

sh "$GISDATA/nation_load.sh"

echo "Generating state TIGER loader for: $STATES"

IFS=',' read -ra STATE_LIST <<< "$STATES"

STATE_SQL="ARRAY["

for state in "${STATE_LIST[@]}"; do
    state=$(echo "$state" | tr '[:lower:]' '[:upper:]' | xargs)
    STATE_SQL="${STATE_SQL}'${state}',"
done

STATE_SQL="${STATE_SQL%,}]"

echo "State array: $STATE_SQL"

$PSQL -At -c \
  "SELECT tiger.loader_generate_script($STATE_SQL, 'sh');" \
  > "$GISDATA/state_load.sh"

chmod +x "$GISDATA/state_load.sh"

sed -i 's/localhost/\/var\/run\/postgresql/g' "$GISDATA/state_load.sh"

sed -i \
  -e '/^export PGBIN=/d' \
  -e '/^export PGUSER=/d' \
  -e '/^export PGPASSWORD=/d' \
  -e '/^export PGDATABASE=/d' \
  "$GISDATA/state_load.sh"

echo "Running state TIGER loader..."

sh "$GISDATA/state_load.sh"

echo "Installing missing indexes..."

$PSQL -c "SELECT tiger.install_missing_indexes();"
echo "========================================"
echo "TIGER geocoder loading complete"
echo "TIGER year: $YEAR"
echo "States: $STATES"
echo "========================================"