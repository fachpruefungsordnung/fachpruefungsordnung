# +------------------------------+
# |            BUILD             |
# +------------------------------+

# Use official Node image to avoid issues with old npm
FROM node:20-bullseye-slim AS build-docs

# install dependencies needed by both build and final
RUN apt-get update && \
    apt-get install -y --no-install-recommends pandoc python3 \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-fonts-recommended \
    texlive-xetex \
    texlive-latex-extra \
    lmodern \
    librsvg2-bin
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

ARG SERVER_HOST="http://localhost"
ENV SERVER_HOST=$SERVER_HOST

WORKDIR /build

COPY docusaurus fpo
COPY docs/ fpo/docs/
COPY scripts/pdf_docs.py /scripts/pdf_docs.py

WORKDIR /build/fpo

# Install dependencies & build
RUN npm install
RUN npm run build

# Replace localhost links with SERVER_HOST
RUN find . -name "*.md" -type f -exec sed -i "s|http://localhost:8080|${SERVER_HOST}|g" {} \;
RUN find . -name "*.html" -type f -exec sed -i "s|http://localhost:8080|${SERVER_HOST}|g" {} \;
RUN find . -name "*.js" -type f -exec sed -i "s|http://localhost:8080|${SERVER_HOST}|g" {} \;

WORKDIR /build/fpo/docs

RUN python3 \
    /scripts/pdf_docs.py \
    /build/fpo/docs \
    /build/fpo/build/fpo-documentation.pdf
#    /build/fpo/docs/template.tex

# +------------------------------+
# |            FINAL             |
# +------------------------------+

FROM python:3.12-slim AS serve-docs

COPY --from=build-docs /build/fpo/build /home/fpo

WORKDIR /home/fpo

CMD ["python", "-m", "http.server", "80"]
