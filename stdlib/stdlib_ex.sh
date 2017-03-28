#!/bin/bash

# Just a small script to build my example
set -euo pipefail

TIMEFORMAT="dhruv:preamble user:%3U sys:%3S real:%3R"
# Root source directory
time SOURCE_ROOT=$1

# Create $SOURCE_ROOT_dpd.v file
time DPD_GEN_FILE="stdlib/DpdStdlib.v"
time rm -f $DPD_GEN_FILE
time touch $DPD_GEN_FILE

# Get all source files
time FILES=$(find $SOURCE_ROOT -name '*.v' | sed 's/\//\./g' | sed 's/.*theories\(.*\)\.v$/Coq\1/g')

# First line
time echo "Require dpdgraph.dpdgraph." >> $DPD_GEN_FILE
time echo '' >> $DPD_GEN_FILE

time echo "$FILES" | sed 's/.*/Require &./g' >> $DPD_GEN_FILE
time echo '' >> $DPD_GEN_FILE

time echo "Set DependGraph File \"DpdStdlib.dpd\"." >> $DPD_GEN_FILE
time echo "Print FileDependGraph" >> $DPD_GEN_FILE
time echo "$FILES" | sed 's/.*/\t&/g' >> $DPD_GEN_FILE
time echo "." >> $DPD_GEN_FILE

DPD_FILE="DpdStdlib.dpd"
time coqc -R . dpdgraph $DPD_GEN_FILE > debug.txt; mv $DPD_FILE stdlib
TIMEFORMAT="dhruv:dpd2csv user:%3U sys:%3S real:%3R"
time ./dpd2 csv -keep-trans stdlib/$DPD_FILE
TIMEFORMAT="dhruv:database user:%3U sys:%3S real:%3R"
time ./build_graph.sh stdlib DpdStdlib stdlib/coqstdlib
