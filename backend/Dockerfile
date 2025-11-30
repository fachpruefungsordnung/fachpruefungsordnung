# check=skip=FromPlatformFlagConstDisallowed
# We skip this because the image does not build currently for arm64
#
# +------------------------------+
# |             BASE             |
# +------------------------------+

FROM --platform=x86_64 debian:bookworm-slim AS base

# install dependencies needed by both build and final
RUN apt-get update && \
    apt-get install -y --no-install-recommends libpq5 locales ca-certificates
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
    gnupg curl libexpat-dev libpq-dev ca-certificates expect \
    build-essential pkg-config libtinfo-dev libncurses-dev
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
COPY README.md README.md

# build project
RUN --mount=type=cache,target=/root/.stack/ \
    --mount=type=cache,target=/build/.stack-work/ \
    stack build --copy-bins --local-bin-path /build/bin\
    --extra-include-dirs=/usr/include/postgresql \
    --extra-lib-dirs=/usr/lib/x86_64-linux-gnu

# +------------------------------+
# |        BUILD HADDOCK         |
# +------------------------------+

FROM build AS build-haddock

# build haddock
RUN --mount=type=cache,target=/root/.stack/ \
    --mount=type=cache,target=/build/.stack-work/ \
    unbuffer stack haddock --no-haddock-deps && \
    DOCROOT=$(stack path --local-install-root)/doc && \
    mkdir -p /out-haddock && \
    cp -r "$DOCROOT"/ /out-haddock/ && \
    ls -R "$DOCROOT"

# +------------------------------+
# |        SERVE HADDOCK         |
# +------------------------------+

FROM python:3.12-slim AS serve-haddock

COPY --from=build-haddock /out-haddock /haddock

WORKDIR /haddock/doc

CMD ["python", "-m", "http.server", "80"]

# +------------------------------+
# |            FINAL             |
# +------------------------------+

FROM base AS final

WORKDIR /home

RUN apt-get update
RUN apt-get install -y --no-install-recommends texlive texlive-latex-extra latexmk texlive-lang-german
RUN apt-get install -y --no-install-recommends wget gnupg2 lsb-release
RUN sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list' \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get update \
    && apt-get install -y postgresql-client-18
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

ENV LANG=de_DE.UTF-8
ENV LC_ALL=de_DE.UTF-8

COPY --from=build /build/bin/backend-exe /home/bin/backend-exe
COPY migrations/ migrations/
COPY static/ static/

ENV MIGRATIONS_DIR=/home/migrations/

# Gute Nacht :)
CMD ["./bin/backend-exe"]
