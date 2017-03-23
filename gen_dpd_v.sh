#!/bin/bash

# Terminate script if a command fails
set -e

# Terminate script if an unset variable is used
set -u

# Root source directory
SOURCE_ROOT=$1

# Create $SOURCE_ROOT_dpd.v file
DPD_GEN_FILE="${SOURCE_ROOT}_dpd.v"
rm -f $DPD_GEN_FILE
touch $DPD_GEN_FILE

# Get all source files
FILES=$(find $SOURCE_ROOT -name '*.v' | sed 's/\//\./g' | sed 's/\(.*\)\.v$/\1/g')

# First line
echo "Require dpdgraph.dpdgraph." >> $DPD_GEN_FILE
echo '' >> $DPD_GEN_FILE

echo "$FILES" | sed 's/.*/Require &./g' >> $DPD_GEN_FILE
echo '' >> $DPD_GEN_FILE

echo "Set DependGraph File \"$SOURCE_ROOT.dpd\"." >> $DPD_GEN_FILE
echo "Print FileDependGraph" >> $DPD_GEN_FILE
echo "$FILES" | sed 's/.*/\t&/g' >> $DPD_GEN_FILE
echo "." >> $DPD_GEN_FILE
