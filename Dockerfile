# Runtime image: docker build -t mutaskell . && docker run --rm -v "$PWD":/code -w /code mutaskell --help
FROM haskell:9.12-slim-bookworm AS build
ENV CABAL_DIR=/root/.cabal
ENV PATH=/root/.cabal/bin:$PATH
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libgmp-dev \
    zlib1g-dev \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
COPY mutaskell.cabal ./
RUN cabal update && cabal build --only-dependencies all
COPY . .
RUN cabal build --write-ghc-environment-files=always all && \
    cabal install --install-method=copy --installdir=/usr/local/bin exe:mutaskell

FROM haskell:9.12-slim-bookworm
ENV CABAL_DIR=/root/.cabal
ENV PATH=/root/.cabal/bin:$PATH
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libgmp-dev \
    zlib1g-dev \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /mutaskell
COPY --from=build /usr/local/bin/mutaskell /usr/local/bin/mutaskell
COPY --from=build /root/.cabal /root/.cabal
COPY --from=build /src /mutaskell

WORKDIR /code
ENTRYPOINT ["mutaskell"]
CMD ["--help"]
