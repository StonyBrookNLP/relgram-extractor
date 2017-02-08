#!/bin/bash
echo "Converting to TuplesAppData to TuplesDocument format"

echo `mvn -o scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.TuplesAppDataTester" -Dexec.args="$1 $2"`
