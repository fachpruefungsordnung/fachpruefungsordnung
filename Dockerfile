# +------------------------------+
# |            BUILD             |
# +------------------------------+

# Use official Node image to avoid issues with old npm
FROM node:20-bullseye-slim AS build-docs

WORKDIR /build

# Install git if you need it (for npx create-docusaurus)
RUN apt-get update && apt-get install -y git && rm -rf /var/lib/apt/lists/*

# Create Docusaurus project non-interactively
RUN npx create-docusaurus@latest fpo classic

# Copy your markdown files into the docs folder
COPY README.md fpo/docs/README.md
COPY docs/ fpo/docs/docs/

WORKDIR /build/fpo

# Install dependencies & build
RUN npm install
RUN npm run build

# The built site is now in /build/fpo/build

# +------------------------------+
# |            FINAL             |
# +------------------------------+

FROM python:3.12-slim AS serve-docs

COPY --from=build-docs /build/fpo /home/fpo

WORKDIR /home/fpo

CMD ["python", "-m", "http.server", "80"]
