#!/bin/bash
echo "Obtaining Relgrams Counts information from TuplesDocumentWithCoreference data"

#echo "`mvn -o -X -e scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.RelgramsLocalApp" -Dexec.args="--equality true --noequality true $1 $2"`"



echo "`mvn -o scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.scoobi.RelgramsExtractorScoobiApp" -Dexec.args="--equality true --noequality true $1 $2"`"
