#!/bin/bash

if [ "$1" == "" ]
then
	echo "Usage: cleanTrace.sh <trace-file>";
	exit 1;
fi;

trace_file=$1;

echo "</events>" >> $trace_file
sed -i 's/&/&amp;/g' $trace_file
# For void pointer type
sed -i 's/__attribute__((__gnu_inline__))  //g' $trace_file
# For void type
sed -i 's/ __attribute__((__gnu_inline__))//g' $trace_file
sed -i 's/  const  / /g' $trace_file
sed -i 's/ const */ /g' $trace_file
sed -i 's/  const//g' $trace_file
