# hadolint ignore=DL3007
FROM toxchat/haskell:latest

RUN ["rm", "-rf", "/work/hs-msgpack-types"]
COPY --chown=builder:users . /work/hs-msgpack-types
RUN ["stack", "build", "msgpack-types"]
