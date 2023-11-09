#!/usr/bin/env sh

set -euxo pipefail

# Build the library
ocamlopt src/plinth.mli -o src/plinth.cmi
ocamlopt src/plinth.ml -a -cmi-file src/plinth.cmi -o src/plinth.cmxa

# Build the tests
ocamlopt -I src src/plinth.cmx test/reformat.ml -o test/reformat

# Run the tests
test/run_tests.sh
