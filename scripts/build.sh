#!/usr/bin/env bash
set -euo pipefail

CONTEXT="fpo-remote-build"

BLACK=$(tput setaf 0)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
LIME_YELLOW=$(tput setaf 190)
POWDER_BLUE=$(tput setaf 153)
BLUE=$(tput setaf 4)
MAGENTA=$(tput setaf 5)
CYAN=$(tput setaf 6)
WHITE=$(tput setaf 7)
BRIGHT=$(tput bold)
NORMAL=$(tput sgr0)
BLINK=$(tput blink)
REVERSE=$(tput smso)
UNDERLINE=$(tput smul)BLACK=$(tput setaf 0)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
LIME_YELLOW=$(tput setaf 190)
POWDER_BLUE=$(tput setaf 153)
BLUE=$(tput setaf 4)
MAGENTA=$(tput setaf 5)
CYAN=$(tput setaf 6)
WHITE=$(tput setaf 7)
BRIGHT=$(tput bold)
NORMAL=$(tput sgr0)
BLINK=$(tput blink)
REVERSE=$(tput smso)
UNDERLINE=$(tput smul)

log() {
  printf "\n[%s] %s\n" "$(date +'%H:%M:%S')" "$1"
}

info() {
  log "[${CYAN}info${NORMAL}] $1"
}

warn() {
  log "[${YELLOW}warn${NORMAL}] $1"
}

if [[ ! -v REMOTE_BUILD_HOST ]]; then
  warn "'REMOTE_BUILD_HOST' not set. Building locally..."
  docker compose build
  exit 0
fi

info "Creating Context..."
docker context rm "${CONTEXT}" &> /dev/null || true
docker context create "${CONTEXT}" \
  --docker "host=ssh://${REMOTE_BUILD_HOST}"

info "Building on '${REMOTE_BUILD_HOST}'..."
DOCKER_BUILDKIT=1 docker --context "${CONTEXT}" compose build

info "Loading Images..."
docker --context ${CONTEXT} compose images --quiet \
  | xargs docker --context "${CONTEXT}" save \
  | docker load

info "Done!"
