# +------------------------------+
# |            BUILD             |
# +------------------------------+

# Use official Node image to avoid issues with old npm
FROM node:20-bullseye-slim AS build-docs

ARG SERVER_HOST="http://localhost"
ENV SERVER_HOST=$SERVER_HOST

WORKDIR /build

COPY docusaurus fpo
COPY docs/ fpo/docs/

WORKDIR /build/fpo

# Install dependencies & build
RUN npm install
RUN npm run build

# +------------------------------+
# |            FINAL             |
# +------------------------------+

FROM python:3.12-slim AS serve-docs

COPY --from=build-docs /build/fpo/build /home/fpo

WORKDIR /home/fpo

CMD ["python", "-m", "http.server", "80"]
