# +------------------------------+
# |            BUILD             |
# +------------------------------+

FROM debian:bookworm-slim AS build

# install dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends locales git npm nodejs python3
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
RUN locale-gen de_DE.UTF-8 && update-locale

# install required npm packages
RUN npm install -g spago@next
RUN npm install -g esbuild
RUN npm install -g purescript

WORKDIR /build

# install project dependencies
COPY package.json package.json
COPY package-lock.json package-lock.json
COPY spago.yaml spago.yaml
COPY spago.lock spago.lock

RUN --mount=type=cache,target=/build/node_modules/ \
    --mount=type=cache,target=/build/output/ \
    npm install && \
    spago install

# build
COPY src src
COPY test test
COPY README.md README.md
COPY module_desc.py module_desc.py
RUN python3 module_desc.py README.md src/FPO/FPO.purs

RUN --mount=type=cache,target=/build/node_modules/ \
    --mount=type=cache,target=/build/output/ \
    spago build && \
    spago bundle --bundle-type app &&\
    spago docs

# +------------------------------+
# |            FINAL             |
# +------------------------------+

FROM nginx:alpine AS final

WORKDIR /docs/dev/purs

COPY --from=build /build/generated-docs/html/ ./

WORKDIR /home

COPY index.html index.html
COPY --from=build /build/index.js index.js

COPY static static
COPY favicon.png favicon.png

COPY docker-entrypoint.sh docker-entrypoint.sh
RUN chmod +x docker-entrypoint.sh

ENTRYPOINT ["./docker-entrypoint.sh"]

CMD ["nginx", "-g", "daemon off;"]
