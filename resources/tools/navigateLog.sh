#!/bin/bash

function usage()
{
	echo "Usage: $(basename $0) [-n <part of file name>]";
	echo -e "\tBy default the tool will list all DOT files in the";
	echo -e "\tcurrent working directory.";
	echo
	echo -e "\tIf -n option is given, this pattern is applied against";
	echo -e "\tall found DOT files and they are filtered accordingly.";
	echo -e "\tExample: $(basename $0) -n xyz";
	echo -e "\tWill list all files with a xyz in their name.";
}

file_name_part=".*";
log_file="log";
log_files_dir=$(mktemp -d)"/";
log_file_suffix=".log";
log_files="*.log";
file_list=();

if [ $# -eq 2 -a "$1" != "-n" ]
then
	usage;
	exit;
else
	file_name_part=$2;
fi;

echo "Working dir: " $log_files_dir;
# Do the parsing/splitting of log file first. parseLogs.pl is found in the DSI repository under resources/tools
/path/to/resources/tools/parseLogs.pl "$(pwd)/$log_file" "$log_files_dir";

dot_files=$(ls $log_files_dir$log_files | grep "$file_name_part");

IFS=$'\n';

for file in $dot_files
do
	file_list+=($file);
done;

file_index=0;
file_list_end=$((${#file_list[@]}-1));
end_loop=0;
if [ $file_list_end -lt 0 ]
then
	file_list_end=0;
	end_loop=1;
fi;
echo "Length of file list: " $file_list_end;

first=1
while [ $end_loop -ne 1 ]
do
	echo "Current file index: " $file_index;
	file=${file_list[$file_index]};

	gvim -f -geometry 500x500+0+0 $file &
	sleep 0.3;
	if [ $first -eq 1 ]
	then
		first=0
		last_xdot_pid=$!;
	else

		echo "File fetched: " $file;
		tmp_last_xdot_pid=$!;
		echo "kill $last_xdot_pid (tmp_last_xdot_pid: $tmp_last_xdot_pid)"
		kill $last_xdot_pid;
		last_xdot_pid=$tmp_last_xdot_pid;
		echo "Last xdot pid: "$last_xdot_pid;
		echo
	fi;

	read -n 1 read_char;
	echo


	if [ "$read_char" == "a" ]
	then
		echo "Go back";
		if [ $file_index -gt 0 ]
		then
			echo "Recalculate index";
			file_index=$(($file_index - 1));
		else
			echo "|<- First file reached.";
		fi;
	elif [ "$read_char" == "d" ]
	then
		echo "Go forward";
		if [ $file_index -lt $file_list_end ]
		then
			echo "Recalculate index";
			file_index=$(($file_index + 1));
		else
			echo "->| Last file reached.";
		fi;
	elif [ "$read_char" == "f" ]
	then
		echo "<< First file";
		file_index=0;
	elif [ "$read_char" == "g" ]
	then
		echo ">> Last file";
		file_index=$file_list_end
	elif [ "$read_char" == "s" ]
	then
		echo "Quit";
		kill $last_xdot_pid;
		end_loop=1;
		# Now always clean up
		echo "Removing $log_files_dir";
		rm -rf $log_files_dir;
		exit 0;
	elif [ "$read_char" == "n" ]
	then
		echo -n "Enter number of index: ";
		read -n 8 read_char;
		# Poor: duplicated code (see else) :(
		requested_index=$(echo $read_char | grep "^[0-9]\+$")
		echo "Requested index: "$requested_index;
		if [ $requested_index -gt 0 -a $requested_index -lt $file_list_end ]
		then
			file_index=$requested_index
			echo "Setting requested index: "$file_index
		fi;
	elif [ "$read_char" == "e" ]
	then
		echo -n "Enter number of event: ";
		read -n 8 read_char;
		# Poor: duplicated code (see above and else) :(
		requested_index=$(echo $read_char | grep "^[0-9]\+$")
		echo "Requested event: "$requested_index;
		index=0;
		for file_name in "${file_list[@]}"
		do
			has_event_id=$(echo $file_name | grep "event_0*$requested_index");
			if [ "$has_event_id" != "" ]
			then
				file_index=$index;
				echo "Setting index to: "$file_index;
				break;
			fi;
			index=$(($index + 1));
		done;
        else
		echo "Read char: "$read_char"."
		requested_index=$(echo $read_char | grep "^[0-9]\+$")
		if [ "$requested_index" != "" ] && [ $requested_index -gt 0 ] && [ $requested_index -lt $file_list_end ]
		then
			echo "Requested index: "$requested_index;
			file_index=$requested_index
			echo "Setting requested index: "$file_index
		fi;
	fi;

done;

# Now always clean up
echo "Removing $log_files_dir";
rm -rf $log_files_dir;
