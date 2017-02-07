export HADOOP_HOME=/home/knowall/hadoop
export grepcmd="egrep '(NYT_ENG_19980918.0012|NYT_ENG_20011203.0021|NYT_ENG_20011218.0072|NYT_ENG_20041109.0007)'"
echo $grepcmd
$HADOOP_HOME/bin/hadoop jar $HADOOP_HOME/contrib/streaming/hadoop-streaming-1.0.2.jar -input $1 -output $2 -mapper "$grepcmd" -reducer NONE -jobconf stream.non.zero.exit.is.failure=false 
