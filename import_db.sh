#!/bin/bash 

# example usage : 
# ./build_graph.sh directory project dependencies

CSVPATH=$1
PREFIX=$2
TARGET=$3

# should grab this from env ... 
NEOHOME="/home/dhruv/Programming/Neo4j/neo4j-community-3.0.5/bin" 

IMPORTARGS="--into $TARGET \
           --nodes:Object $CSVPATH/$PREFIX\_node.csv \
           --relationships:USES $CSVPATH/$PREFIX\_edge.csv"

# echo $IMPORTARGS
# make sure neo4j database is empty 
rm -rf $TARGET/*
# invoke the CSV import tool 
$NEOHOME/neo4j-import $IMPORTARGS
            


