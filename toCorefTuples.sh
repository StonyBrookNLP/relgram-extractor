#!/bin/bash
echo "Converting to TuplesDocument to TuplesDocumentWithCoreference format"

echo `mvn -o scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.CoreferenceTuples" -Dexec.args="$1 $2"`
