#!/usr/bin/env sh

set -euxo pipefail

warnings="-w A-42-70-30"

# Build the library
ocamlopt $warnings src/plinth.mli -o src/plinth.cmi
ocamlopt $warnings src/plinth.ml -a -cmi-file src/plinth.cmi -o src/plinth.cmxa

# Build the tests
ocamlopt $warnings -I src src/plinth.cmx test/reformat.ml -o test/reformat
ocamlopt $warnings -I src src/plinth.cmx test/type_of.ml -o test/type_of
ocamlopt $warnings -I src src/plinth.cmx test/stack_instrs.ml -o test/stack_instrs

# Run the tests
test/run_tests.sh
