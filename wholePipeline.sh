#!/bin/bash
echo "Running the entire pipeline"

echo `mvn -o scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.TuplesAppDataTester" -Dexec.args="$1 outfile_1"`
#echo `mvn -o scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.CoreferenceTuples" -Dexec.args="outfile_1 the_test"`
echo "`mvn -o scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.scoobi.TuplesDocumentsWithCorefScoobiApp" -Dexec.args="--fromDocs true outfile_1 none coref_docs"`"  
#echo `mvn -o scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.RelgramsLocalApp" -Dexec.args="--equality true --noequality true the_test $2"`

#echo "`mvn -o scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.scoobi.RelgramsExtractorScoobiApp" -Dexec.args="--equality true --noequality true the_test $2"`"
