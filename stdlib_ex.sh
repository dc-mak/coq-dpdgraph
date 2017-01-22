#!/bin/bash

# Just a small script to build my example
set -euo pipefail

echo "Analysing Coq project (seed debug.txt)"
coqc -R . dpdgraph DpdStdlib.v > debug.txt
./dpd2csv -keep-trans DpdStdlib.dpd
./build_graph.sh . DpdStdlib ./coqstdlib
