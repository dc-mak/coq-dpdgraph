#!/bin/bash

# Just a small script to build my example
set -euo pipefail

# Root source directory
SOURCE_ROOT=$1

# Create $SOURCE_ROOT_dpd.v file
DPD_GEN_FILE="DpdStdlib_upstream.v"
rm -f $DPD_GEN_FILE
touch $DPD_GEN_FILE

# Get all source files
FILES=$(find $SOURCE_ROOT -name '*.v' | sed 's/\//\./g' | sed 's/.*theories\(.*\)\.v$/Coq\1/g')

# First line
echo "Require dpdgraph.dpdgraph." >> $DPD_GEN_FILE
echo '' >> $DPD_GEN_FILE

echo "$FILES" | sed 's/.*/Require &./g' >> $DPD_GEN_FILE
echo '' >> $DPD_GEN_FILE

echo "Set DependGraph File \"DpdStdlib_upstream.dpd\"." >> $DPD_GEN_FILE
echo "Print FileDependGraph" >> $DPD_GEN_FILE
echo "$FILES" | sed 's/.*/\t&/g' >> $DPD_GEN_FILE
echo "." >> $DPD_GEN_FILE

coqc -R . dpdgraph DpdStdlib_upstream.v
