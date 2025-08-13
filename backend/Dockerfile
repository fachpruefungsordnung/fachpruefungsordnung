# +------------------------------+
# |             BASE             |
# +------------------------------+

FROM debian:bookworm-slim AS base

# install dependencies needed by both build and final
RUN apt-get update && \
  apt-get install -y --no-install-recommends libpq5 locales
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
RUN locale-gen de_DE.UTF-8 && update-locale

# +------------------------------+
# |            BUILD             |
# +------------------------------+

FROM base AS build

# install build tools
RUN apt-get update
RUN apt-get install -y --no-install-recommends \
  g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git \
  gnupg curl libexpat-dev libpq-dev ca-certificates
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# install stack
RUN curl -sSL https://get.haskellstack.org/stable/linux-x86_64.tar.gz | \
  tar -xz --wildcards --strip-components=1 -C \
  /usr/local/bin 'stack-*-linux-x86_64/stack'

WORKDIR /build

# copy project files
COPY stack.yaml stack.yaml
COPY stack.yaml.lock stack.yaml.lock
COPY package.yaml package.yaml
COPY *.cabal ./

# build dependencies
RUN --mount=type=cache,target=/root/.stack/ \
  --mount=type=cache,target=/build/.stack-work/ \
  stack setup && stack build --only-dependencies \
  --extra-include-dirs=/usr/include/postgresql \
  --extra-lib-dirs=/usr/lib/x86_64-linux-gnu

# copy source files
COPY src/ src/
COPY app/ app/

# build project
RUN --mount=type=cache,target=/root/.stack/ \
--mount=type=cache,target=/build/.stack-work/ \
stack build --copy-bins --local-bin-path /build/bin\
  --extra-include-dirs=/usr/include/postgresql \
  --extra-lib-dirs=/usr/lib/x86_64-linux-gnu

# +------------------------------+
# |            FINAL             |
# +------------------------------+

FROM base AS final

WORKDIR /home

RUN apt-get update
RUN apt-get install -y --no-install-recommends texlive
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

COPY --from=build /build/bin/backend-exe /home/bin/backend-exe
COPY migrations/ migrations/
COPY static/ static/

ENV MIGRATIONS_DIR=/home/migrations/

# Gute Nacht :)
CMD ["./bin/backend-exe"]
