# Development image: docker build -f dev.Dockerfile -t mutaskell-dev . && docker run --rm -it -v "$PWD":/workspace mutaskell-dev
FROM haskell:9.12-slim-bookworm
ENV CABAL_DIR=/root/.cabal
ENV PATH=/root/.cabal/bin:$PATH
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libgmp-dev \
    zlib1g-dev \
    git \
    bash \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace
COPY mutaskell.cabal ./
RUN cabal update && cabal build --only-dependencies --enable-tests all
COPY . .
RUN cabal build --write-ghc-environment-files=always --enable-tests all
CMD ["cabal", "test", "--write-ghc-environment-files=always", "all", "--test-show-details=direct"]
