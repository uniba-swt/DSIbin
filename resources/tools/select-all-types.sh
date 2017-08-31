#!/bin/bash

path=$1;

for dir in `ls -d $path/*/`;
do
	echo $dir;

	proj_name=`basename $dir`;
	profile_file=$path$proj_name".msrprf";

	echo "./select-interpretation.pl $dir/dsi-refined-types/ | sed -n -e '/with nodes count/,\$p' | grep labels | sed 's/ /,/g' | sort -t, -n -k4";
	./select-interpretation.pl $dir/dsi-refined-types/ | sed -n -e '/Print clustered/,$p'
	echo "*****************************************************************";
done;
