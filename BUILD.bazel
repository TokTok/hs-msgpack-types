load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
    name = "hs-msgpack-types",
    srcs = glob(["src/**/*.*hs"]),
    prebuilt_dependencies = [
        "base",
        "bytestring",
        "containers",
        "deepseq",
    ],
    src_strip_prefix = "src",
    visibility = ["//visibility:public"],
    deps = [
        "@haskell_QuickCheck//:QuickCheck",
        "@haskell_hashable//:hashable",
        "@haskell_text//:text",
        "@haskell_unordered_containers//:unordered-containers",
        "@haskell_vector//:vector",
    ],
)
