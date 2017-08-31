#!/bin/bash

# Note: Please adjust the two paths below.
# The first path points to the xml folder in the DSIbin resources folder
# The second path points to the location of DSI's Pin module, i.e., malloctrace.so



path=$1;

for dir in `ls -d $path/*/`;
do
	echo $dir;

	proj_name=`basename $dir`;
	profile_file=$path$proj_name".msrprf";

	/usr/bin/time -v -o $profile_file ./type-refinement.pl  $dir /path/to/DSI-bin/resources/xml/ /path/to/DSI-bin-inst/obj-intel64/ $proj_name;

done;
