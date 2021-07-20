#!/usr/bin/env bash
set -ue

LC_ALL=C

cd $(dirname "$0")/..

## Run build in container

tar c $(git ls-files) | \
  docker run --rm -i \
    ocamlpro/ocaml:4.05 \
    sh -uexc \
      'tar x >&2 &&
       sudo apk add openssl-libs-static >&2 &&
       opam switch create . ocaml-system "dune<2" --deps-only >&2 &&
       opam exec make LINKING_MODE=static >&2 &&
       tar c -hC _build/install/default/bin .' | \
  tar vx
