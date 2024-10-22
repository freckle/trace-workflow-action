FROM haskell:9.2.7 AS build

WORKDIR /opt/trace-workflow-action
COPY stack.yaml stack.yaml.lock package.yaml trace-workflow.cabal .
RUN stack build --only-dependencies

COPY app ./app
COPY src ./src

RUN mkdir bin && stack --local-bin-path ./bin build --copy-bins

FROM debian:bullseye

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /opt/trace-workflow-action/bin/trace-workflow /trace-workflow

ENTRYPOINT ["/trace-workflow"]
