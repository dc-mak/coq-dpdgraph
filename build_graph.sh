#!/bin/bash 

# usage : 
# ./build_graph.sh <neo4j path> <csv path> <database name (directory)> 
# example: 
# ./build_graph.sh ~/working/lectures/db1A/ne04j/neo4j-community-3.0.4/bin . graph-db 
#

NEOHOME=$1  # should grab this from env? 
CSVPATH=$2
TARGET=$3

IMPORTARGS="--into $TARGET \
           --nodes test_node.csv \
           --relationships test_edge.csv" 

# make sure neo4j database is empty 
rm -rf $TARGET/*
# invoke the CSV import tool 
$NEOHOME/neo4j-import --delimiter "," $IMPORTARGS
            


