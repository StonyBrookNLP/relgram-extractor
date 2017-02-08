#!/bin/bash
echo "Obtaining Relgrams Counts information from TuplesDocumentWithCoreference data"

echo `mvn -o scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.RelgramsLocalApp" -Dexec.args="--equality true --noequality true $1 $2"`
