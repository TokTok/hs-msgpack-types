FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-msgpack-types"]
COPY --chown=builder:users . /work/hs-msgpack-types
RUN ["stack", "install", "msgpack-types"]

FROM scratch
COPY --from=build /home/builder/.local/ /
