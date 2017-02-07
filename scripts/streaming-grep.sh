export HADOOP_HOME=/home/knowall/hadoop
$HADOOP_HOME/bin/hadoop jar $HADOOP_HOME/contrib/streaming/hadoop-streaming-1.0.2.jar -input $1 -output $2 -mapper "$3" -reducer NONE -jobconf stream.non.zero.exit.is.failure=false 
