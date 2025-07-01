#!/usr/bin/sh
set -eu

TEMPLATE=/etc/nginx/templates/ssl.conf.template

if [ -e "$TEMPLATE" ]; then
    envsubst '${HOST_URL}' < "$TEMPLATE" > /etc/nginx/conf.d/ssl.conf
fi

exec "$@"
