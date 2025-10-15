---
sidebar_position: 2
---

# Configure

All services are orchestrated using Docker Compose.

Deployment of the service should happen by using the `docker-compose.ssl.yaml` as an
override to have SSL properly set up, see the appropriate section below

## Technologies

- [Docker](https://www.docker.com/)
- [PostgreSQL](https://www.postgresql.org/) for the database
- [pgweb](https://sosedoff.github.io/pgweb/) as a database client
- [nginx](https://nginx.org/)

## Overview

- the frontend user interface is available at `/`, e.g., [/](http://localhost:8080/)
- the backend api is available at `/api/`, e.g., [/api/](http://localhost:8080/api/)
- internally, the services are reachable by their respective hostnames:
    - Postgres: `postgres`
    - Backend: `api`
- a collection of development tools and links is available at `/dev/`, e.g., [/dev/](http://localhost:8080/dev/)
    - a simple database client is available at `/pgweb/`, e.g., [/pgweb/](http://localhost:8080/pgweb/). For deployment, this should be password protected, see [Environment](#Environment)
    - the API documentation is available at `/swagger/`, e.g., [/swagger/](http://localhost:8080/swagger/)
    - the documentation hosted in its entirety at `/docs/`, e.g. [/docs/](http://localhost:8080/docs/)
        - the hosted backend documentation is available at `/dev/haddock/`, e.g. [/dev/haddock/](http://localhost:8080/dev/haddock/)
        - the hosted frontend documentation is available at `/dev/purs/`, e.g. [/dev/purs/](http://localhost:8080/dev/purs/)
    - backend logs can be found at `dev/logs.html`, e.g. [/dev/logs.html](http://localhost:8080/dev/logs.html). You can login here with any credentials that are also superadmins within the application.

## Quick Setup

1. Install Docker.
2. Create a `.env` file similar to `.env.example` in the root directory.
3. Start all services via `docker compose up`.

## Environment

The `.env` file should contain all settings.
An exemplary `.env` file for can be found in the root directory within `.env.example`.
The `.env` file should set the following environment variables:

### General

- `PORT` defines the port at which the web application will be available

### Postgres

- `POSTGRES_DB` defines the name of the postgres database
- `POSTGRES_USER` defines the name of the postgres user account
- `POSTGRES_PASSWORD` defines the password for the postgres user account

### Mail configuration (optional)

These secrets are needed if you want to use the mail service within the backend. Currently,
only STARTTLS is supported.

- `MAIL_ADDRESS` defines the mail address that the mails will be sent from
- `MAIL_HOST` defines the host for the mail server
- `MAIL_PORT` defines the STARTTLS port of the mail server
- `MAIL_USERNAME` defines the username for the specified mail servef
- `MAIL_PASSWORD` defines the password for the specified user

### Pgweb (optional)

The following settings enable basic auth for pgweb.
To disable basic auth, leave these variables unset.
In production, these should be set.

- `PGWEB_AUTH_USER` defines the username for the pgweb database client
- `PGWEB_AUTH_PASS` defines the password for the pgweb database client


## Certbot and SSL

To set up SSL certificates, you can use `scripts/install-certs` to request a
certificate from the certificate authority and `scripts/renew-certs` to update these.
Note you have to set up the certificates once manually yourself using these scripts.
The certificates are then used by the Nginx container in a production environment
by using the `docker-compose.ssl.yaml` override. You can invoke the override via

```sh
docker compose -f docker-compose.yaml -f docker-compose.ssl.yaml up --build
```

`scripts/renew-certs` should optimally be set up to be executed by a cronjob periodically.
