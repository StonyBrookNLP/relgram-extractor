nohup mvn -o scala:compile exec:java -Dexec.mainClass="edu.washington.cs.knowitall.relgrams.web.RelgramsViewerFilter" -Dexec.args="http://rv-n15.cs.washington.edu:10000/solr/apr02-relgrams http://rv-n15.cs.washington.edu:10000/solr/tupledocuments-withconf src/main/resources/relgrams.html --port 10000 --host relgrams.cs.washington.edu" &> emnlp-relgrams-web-release.out &