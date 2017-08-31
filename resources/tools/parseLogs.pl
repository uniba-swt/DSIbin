#!/usr/bin/perl
use strict;
use warnings;

# Script parses the dsOli log file

my $num_args = $#ARGV + 1;

if ($num_args != 2) {
	print "Usage: parseLogs.pl source-file log-path-dir\n";
	exit(1);#
}

my $source_file=$ARGV[0];
open(my $sfh, "<", $source_file) or die "Unable to open file " . $source_file. ": $!";

my $log_path_dir=$ARGV[1]; #"/tmp/dsolilogs/";
my $log_file_prefix="event_";
my $log_file_suffix=".log";
my $state_search_method = 0;
my $state_search_open = 1;
my $cur_state = $state_search_method;
my $braces = 0;
my $found_debug = 0;
my $line_buffer = "";
my $method_name = "";
my $cur_event = 0;
while(my $line = <$sfh>) {

	if ($cur_state == $state_search_method) {
#print $line;
		if($line =~ /#Event\s*([0-9]+)\s*#/){
			$cur_event=$1;
			$line_buffer=$line;
			$cur_state = $state_search_open;
#print "Found event: " . $1;
		}
	}
	elsif($cur_state == $state_search_open) {
		$line_buffer = $line_buffer . $line;
		if($line =~ /#Done\s+Event\s+([0-9]+)\s*#/) {
#print "Found end: " . $cur_event;
			my $filename = $log_path_dir . $log_file_prefix . sprintf("%08d",$cur_event) . $log_file_suffix;
			open(my $fh, '>>', $filename) or die "Could not open file '$filename' $!";
			print $fh $line_buffer;
			close $fh;
			$cur_state=$state_search_method;
#print $line_buffer;
		}
	}
}

if ($cur_state != $state_search_method) {
	print "Parsing not properly finished. Dumping to last found event: ". $cur_event . "\n";
	my $filename = $log_path_dir . $log_file_prefix . sprintf("%08d",$cur_event) . $log_file_suffix;
	open(my $fh, '>>', $filename) or die "Could not open file '$filename' $!";
	print $fh $line_buffer;
	close $fh;
}

close($sfh);
