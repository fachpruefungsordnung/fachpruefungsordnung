---
sidebar_position: 2
---

# Configure

All services are orchestrated using docker compose.

## Technologies

- [Docker](https://www.docker.com/)
- [PostgreSQL](https://www.postgresql.org/) for the database
- [pgweb](https://sosedoff.github.io/pgweb/) as a database client
- [nginx](https://nginx.org/)

## Overview

- the frontend user interface is available at `/`, e.g., [localhost:8080](http://localhost:8080/) for local development
- a collection of development tools and links is available at `/dev/`, e.g., [localhost:8080/dev/](http://localhost:8080/dev/)
- a simple database client is available at `/pgweb/`, e.g., [localhost:8080/pgweb/](http://localhost:8080/pgweb/) for local development. For deployment, this should be password protected, see [Environment](#Environment)
- the api documentation is available at `/swagger/`, e.g., [localhost:8080/swagger](http://localhost:8080/swagger/)
- the backend api is available at `/api/`, e.g., [localhost:8080/api/](http://localhost:8080/api/) for local development
- internally, the services are reachable by their respective hostnames:
    - Postgres: `postgres`
    - Backend: `api`

## Quick Setup

- install Docker
- create a `.env` file similar to `.env.example` in the root directory
- start all services via `docker compose up`

## Environment

The `.env` file should contain all settings.
An exemplary `.env` file for can be found in the root directory.
The `.env` file should set the following environment variables:

### General

- `PORT` defines the port at which the web application will be available

### Postgres

- `POSTGRES_DB` defines the name of the postgres database
- `POSTGRES_USER` defines the name of the postgres user account
- `POSTGRES_PASSWORD` defines the password for the postgres user account

### Pgweb (optional)

The following settings enable basic auth for pgweb.
To disable basic auth, leave these variables unset.
In production, these should be set.

- `PGWEB_AUTH_USER` defines the username for the pgweb database client
- `PGWEB_AUTH_PASS` defines the password for the pgweb database client
