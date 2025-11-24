#!/usr/bin/env bash
set -euo pipefail

source .env

REMOTE_USER="fpo"
REMOTE_HOST="batailley.informatik.uni-kiel.de"
REMOTE_PORT="22"

PG_CONTAINER="fpo-postgres"

DB_NAME="fpo"
DB_USER="einuser"

REMOTE_TMP_DIR="/tmp"

LOCAL_BACKUP_DIR="$HOME/fpo-backups/postgres"

TIMESTAMP=$(date +"%Y-%m-%d_%H-%M-%S")
DUMP_FILENAME="backup_${DB_NAME}_${TIMESTAMP}.sql.gz"
REMOTE_DUMP_PATH="${REMOTE_TMP_DIR}/${DUMP_FILENAME}"

mkdir -p "$LOCAL_BACKUP_DIR"

echo "[INFO] Starte Backup von ${DB_NAME}@${REMOTE_HOST} (Docker)"

echo "[INFO] Erzeuge Dump im Docker-Container..."
ssh -p "$REMOTE_PORT" "${REMOTE_USER}@${REMOTE_HOST}" \
  "docker exec ${PG_CONTAINER} pg_dump -U ${DB_USER} ${DB_NAME} | gzip > '${REMOTE_DUMP_PATH}'"

echo "[INFO] Lade Dump herunter..."
scp -P "$REMOTE_PORT" "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_DUMP_PATH}" \
    "${LOCAL_BACKUP_DIR}/${DUMP_FILENAME}"

echo "[INFO] Entferne temporären Dump auf Remote..."
ssh -p "$REMOTE_PORT" "${REMOTE_USER}@${REMOTE_HOST}" \
  "rm -f '${REMOTE_DUMP_PATH}'"

echo "[INFO] Backup abgeschlossen: ${LOCAL_BACKUP_DIR}/${DUMP_FILENAME}"
