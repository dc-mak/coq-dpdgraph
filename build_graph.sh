#!/bin/bash 
set -e # Fail immediately if any command fails
set -u # Fail if any variables are uninitialised

# usage : 
# ./build_graph.sh <dir-of-csv-files> <name-of-database-dir> 
# example: 
# ./build_graph.sh . graph-db 

CSVPATH=$1
CSVPREFIX=$2
TARGET=$3

IMPORTARGS="--into $TARGET \
           --nodes $CSVPATH/$CSVPREFIX\_node.csv \
           --relationships $CSVPATH/$CSVPREFIX\_edge.csv" 

# Make sure target Neo4j database is empty 
rm -rf $TARGET/*

# Invoke the CSV import tool 
$NEO4J_BIN_DIR/neo4j-import --delimiter "," $IMPORTARGS
