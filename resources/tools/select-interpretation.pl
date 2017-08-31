#!/usr/bin/perl

use warnings;
use strict;

use File::Basename;
#

# Sanity check of command line
my $num_args = $#ARGV + 1;
if ($num_args != 1) {
    print "Usage: select-interpretation.pl <absolute-path-to-dsi-refined-types-folder>\n";
    exit;
}

my $refined_ds_path=$ARGV[0];
my @labels = ("SLo", "SLi", "BT", "CDLL", "DLL", "No", "Ni"); #, "SHN", "I2o", "I1o");
#my @labels = ("SLo", "SLi", "BT", "CDLL", "DLL", "No", "Ni", "SHN", "I2o", "I1o");
#my @labels = ("SLo", "SLi", "BT", "CDLL", "DLL", "SHN", "I2o", "I1o");
#my @labels = ("SLo", "SLi", "BT", "CDLL", "DLL", "SHN", "I2o", "I1o", "ccNoClassification");
#my @nestings = ("No", "Ni");
my @nestings = ();

my @dirs = glob "$refined_ds_path/logtypes_*";

## Helper
sub first_label_higher {
	my ($first_label, $second_label) = @_;
	my $first_label_index = find_label_in_hierarchy($first_label);
	my $second_label_index = find_label_in_hierarchy($second_label);
	# Labels are sorted descending, with the highest label at the first
	# position in the labels array => a label with a lower index has
	# higher precedence. If indices are equal return false
	return $first_label_index < $second_label_index;

}

sub find_label_in_hierarchy {
	# Uses the global label array and iterates it to find
	# a match with the label_to_check. The found index is
	# returned, or otherwise labels length plus one, as this
	# makes it lowest precedence
	my $label_to_check = shift;

	if(! defined($label_to_check) ) {
		$label_to_check = "";
	}

	for my $i (0 .. $#labels) {
		my $cur_label = $labels[$i];
		print "ckecking $i: $labels[$i]\n";
		if($cur_label eq $label_to_check) {
			return $i;
		}
	}
	print "nothing found: returning: " . ($#labels + 1) . "\n";
	return $#labels + 1;
}
## Helper End


# Multiple nested hash maps:
# First level: key: hypothesis => list of hash maps ( key: longest running ep => list of labels )
my $hypothesis_mapping = {};

print "*********************** Select longest running eps with labels\n";
# Iterate over all dirs which represent one type hypothesis
for (0..$#dirs) {
	my $cur_hypothesis = $dirs[$_];
	print $cur_hypothesis . ":\n";

	# Iterate over all aggregate graphs and collect the longest running eps
	my @files = glob $dirs[$_]."/agg/*cleaned*";
	my @longestRunningEp = ();
	my @longestRunningEpFile = ();
	my $longestRunningEpVal = 0;
	for (0..$#files){
		my $file = $files[$_];
		my $grep_result = `grep aggCnt $file | sed -e 's/^.*ep: //' -e 's/ start.*aggCnt: /;/' -e 's/<.*//'`;
		my ($ep, $ep_cnt) = split(/;/, $grep_result);

		if($longestRunningEpVal < $ep_cnt) {
			@longestRunningEp = ();
			@longestRunningEpFile = ();
			$longestRunningEpVal = $ep_cnt;
		}
		if($longestRunningEpVal <= $ep_cnt) {
			push(@longestRunningEp, $ep);
			push(@longestRunningEpFile, $file);
		}
	}


	# Iterate over the longest running eps and select the labels from the corresponding aggregate graph
	# Additionally choose the most complex interpretation among the longest eps: if equally complex ->
	# keep the first stored ep.
	print "All eps with aggregation count: " . $longestRunningEpVal . "\n";
	my $chosen_label_count = 0;
	my $chosen_longest_ep = "";
	my $chosen_longest_ep_file = "";
	my @chosen_longest_ep_labels = ();
	my @chosen_longest_ep_nestings = ();
	for(0..$#longestRunningEp) {

		my $ep = $longestRunningEp[$_];
		print "ep: " . $longestRunningEp[$_] . " in file: " . $longestRunningEpFile[$_] . "\n";
		my $file = $longestRunningEpFile[$_];
		my $cur_label_count=0;

		# Select the labels found for this ep
		my @ep_labels = ();
		for(0..$#labels){
			my $label = $labels[$_];
			my $refined_label = $label . "[^C]";
			# Prevent matching of DLL in the presence of CDLL with [^C]
			if($refined_label eq "DLL") {
				$refined_label = "[^C]" . $refined_label;
				print "refining label: " . $refined_label . "\n";
			}
			my $grep_label = `grep -c $refined_label $file`;
			print "grep result: " . $grep_label . "\n";
			if($grep_label == 0) {
				print "grep: " . $label . " count: empty\n";
			} else {
				print "grep: " . $label . " count: " . $grep_label . "\n";
				push(@ep_labels, $label);
				my $cur_label_count_cmd = 'grep ' . $refined_label . ' ' . $file . ' | sed -e \'s/.*>' . $label . '//\' -e \'s/<\/TD><TD ALIGN="left" BGCOLOR="#[0-9A-F]*">/ /\' -e \'s/(.*//\' ';
				print "cur_label_count_cmd: " . $cur_label_count_cmd . "\n";
				my $cur_label_counts = `$cur_label_count_cmd`;
				my @label_counts= split(/\n/,$cur_label_counts);


				$cur_label_count = 0;
				for my $count (@label_counts) {
					print "\textracted count: ." . $count . ".\n";
					if($cur_label_count < $count){
						$cur_label_count = $count;
					}
				}
				print "highest count: " . $cur_label_count . "\n";


			}

		}

		# If no label was found, insert a defined undef value
		# for the rest of the script
		if($#ep_labels == -1) {
			push(@ep_labels, "undef");
		}

		# Select the nestings found for this ep
		my @nesting_labels = ();
		for(0..$#nestings){
			my $cur_nesting = $nestings[$_];
			# Prevent matching of ccNoClassification with [^C]
			my $nesting = $cur_nesting . "[^C]";
			my $grep_nesting = `grep -c $nesting $file`;
			if($grep_nesting == 0) {
				print "grep: " . $nesting . " count: empty\n";
			} else {
				print "grep: " . $nesting . " count: " . $grep_nesting . "\n";
				push(@nesting_labels, $cur_nesting);
			}
		}

		# Choose the most complex interpretation among the longest running eps
		if($chosen_longest_ep eq "") {
			# Init
			$chosen_label_count = 0;
			$chosen_longest_ep = $ep;
			$chosen_longest_ep_file = $file;
			@chosen_longest_ep_labels = @ep_labels;
			@chosen_longest_ep_nestings = @nesting_labels;
		} else {
			# Compare the labels according the hierarchy

			# Choose the first label we have found for this ep
			my $cur_first_label = $ep_labels[0];

			# Choose the first label of the previously selected longest running ep
			my $chosen_first_label = $chosen_longest_ep_labels[0];

			print "Comparing: cur_first_label: " . $cur_first_label . " chosen_first_label: " . $chosen_first_label . "\n";

			# If the cur label is higher, make the current label the chosen one OR
			# if the cur labels are equal AND the current label count is higher than the chosen one
			if(first_label_higher($cur_first_label, $chosen_first_label) ||
			($cur_first_label eq $chosen_first_label &&
				 $cur_label_count > $chosen_label_count)){
				print "\t cur_first_label higher than chosen_first_label\n";
				$chosen_label_count = $cur_label_count;
				$chosen_longest_ep = $ep;
				$chosen_longest_ep_file = $file;
				@chosen_longest_ep_labels = ();
				@chosen_longest_ep_nestings = ();
				@chosen_longest_ep_labels = @ep_labels;
				@chosen_longest_ep_nestings = @nesting_labels;
			}
		}

	}

	# Store the longest running ep
	if(!exists($hypothesis_mapping->{$cur_hypothesis})){
		# Create a record for the currently collected data
		my %cur_hypothesis_record = (
			"file" => $chosen_longest_ep_file,
			"labels" => \@chosen_longest_ep_labels,

			"nesting" => \@chosen_longest_ep_nestings
		);
		# Add the record to the longest running ep
		$hypothesis_mapping->{$cur_hypothesis} = {};
		$hypothesis_mapping->{$cur_hypothesis}->{$chosen_longest_ep} = \%cur_hypothesis_record;
	}

}

my $most_complex_label = "";
my $most_complex_nesting = "";
print "*********************** Choose hypotheses\n";
my $chosen_hypotheses = {};
foreach my $key ( keys %{ $hypothesis_mapping } )
{
	my $hypothesis = $hypothesis_mapping->{$key};
	print "key: $key\n";
	foreach my $ep_key ( keys %{ $hypothesis } ) {
		print "\tep: $ep_key\n";
		my $ep_values = $hypothesis_mapping->{$key}->{$ep_key};
		print "\t\tfile: ".$ep_values->{file}."\n";

		my $cur_labels = $ep_values->{labels};
		my $cur_nestings = $ep_values->{nesting};
		print "\t\tlabels: ";
		foreach my $label (@{$cur_labels}) {
			print $label . ","
		}
		print "\n";

		if(defined $cur_nestings) {
			print "\t\tnesting: ";
			foreach my $label (@{$cur_nestings}) {
				print $label . ",";
			}
			print "\n";
		} else {
			print "\t\tnesting: none\n";
		}

		print "investigating: " . $ep_key . ", " . $ep_values . "\n";
		if($most_complex_label eq ""){
			print "Most complex label: init\n";
			$most_complex_label = @{$cur_labels}[0];
			$most_complex_nesting = @{$cur_nestings}[0];
			print "Most complex label: $most_complex_label, nesting: $most_complex_nesting\n";
			# Reset all previously selected hypotheses and append current one
			$chosen_hypotheses = {};
			$chosen_hypotheses->{$key} = {};
			$chosen_hypotheses->{$key}->{$ep_key} = $ep_values;
		} elsif (first_label_higher(@{$cur_labels}[0], $most_complex_label)) {
			print "Most complex label: change: $most_complex_label -> @{$cur_labels}[0]\n";
			$most_complex_label = @{$cur_labels}[0];
			$most_complex_nesting = @{$cur_nestings}[0];
			# Reset all previously selected hypotheses and append current one
			$chosen_hypotheses = {};
			$chosen_hypotheses->{$key} = {};
			$chosen_hypotheses->{$key}->{$ep_key} = $ep_values;
		} elsif (
			(! defined $most_complex_nesting && defined @{$cur_nestings}[0]) ||
			($most_complex_nesting eq "Ni" && defined @{$cur_nestings}[0] && @{$cur_nestings}[0] eq "No")){
			print "Most complex label: nesting change: $most_complex_nesting -> @{$cur_nestings}[0]\n";
			$most_complex_label = @{$cur_labels}[0];
			$most_complex_nesting = @{$cur_nestings}[0];
			# Reset all previously selected hypotheses and append current one
			$chosen_hypotheses = {};
			$chosen_hypotheses->{$key} = {};
			$chosen_hypotheses->{$key}->{$ep_key} = $ep_values;
		} elsif ($most_complex_label eq @{$cur_labels}[0] && $most_complex_nesting eq @{$cur_nestings}[0]) {
			# Append the current key
			$chosen_hypotheses->{$key} = {};
			$chosen_hypotheses->{$key}->{$ep_key} = $ep_values;
		}

		if(! defined($most_complex_label)) {
			$most_complex_label = "";
		}

		if(! defined($most_complex_nesting)) {
			$most_complex_nesting = "";
		}

	}
}

print "*********************** Select lowest amount of nodes\n";
# Select the smallest amount of nodes
my $smallest_amount_of_nodes = -1;
foreach my $key ( keys %{ $chosen_hypotheses } )
{
	my $hypothesis = $chosen_hypotheses->{$key};

	foreach my $ep_key ( keys %{ $hypothesis } ) {
		my $ep_values = $hypothesis_mapping->{$key}->{$ep_key};
		my $file = $ep_values->{file};
		my $grep_nodes_cmd = 'grep "DsOliMetaBoxGraphVertexBoxes.*\[label =.*TABLE" '. $file.' | wc -l';
		my $nodes_cnt = `$grep_nodes_cmd`;

		if($smallest_amount_of_nodes == -1){
			$smallest_amount_of_nodes = $nodes_cnt;
		} elsif($nodes_cnt < $smallest_amount_of_nodes){
			$smallest_amount_of_nodes = $nodes_cnt;
		}
	}
}

print "*********************** Printing hypotheses with nodes count: $smallest_amount_of_nodes\n";


# cluster the hypotheses based on
my $clustered_hypotheses = {};

foreach my $key ( keys %{ $chosen_hypotheses } )
{
	my $hypothesis = $chosen_hypotheses->{$key};
	print "key: $key\n";


	# Calculate taint file name and path
	my $hypothesis_name = basename($key);
	my $hypothesis_number = $hypothesis_name;
	$hypothesis_number =~ s/logtypes_//;
	my $hypothesis_taint = "types_" . $hypothesis_number . ".taint";
	my $hypothesis_taint_path = $refined_ds_path . $hypothesis_taint;

	# Get the type instances count and the type instances themselfs
	my $regex_struct = "struct[^{]";
	my $grep_structs_cnt_cmd = "grep  $regex_struct $hypothesis_taint_path | wc -l";
	my $grep_structs_cmd     = "grep  $regex_struct $hypothesis_taint_path";
	my $structs_cnt = `$grep_structs_cnt_cmd`;
	my $structs = `$grep_structs_cmd`;

	print "type instances count: " . $structs_cnt . "\n";
	print "type instances:" . $structs. "\n";

	foreach my $ep_key ( keys %{ $hypothesis } ) {
		print "\tep: $ep_key\n";
		my $ep_values = $hypothesis_mapping->{$key}->{$ep_key};
		my $file = $ep_values->{file};

		my $grep_nodes_cmd = 'grep "DsOliMetaBoxGraphVertexBoxes.*\[label =.*TABLE" '. $file.' | wc -l';
		my $nodes_cnt = `$grep_nodes_cmd`;
		if($nodes_cnt > $smallest_amount_of_nodes){
			print "\t-> Skipping because number of nodes is too high: $nodes_cnt > $smallest_amount_of_nodes\n";
		}
		$nodes_cnt =~ s/^\s+//;
		$nodes_cnt =~ s/\s+$//;
		print "\tnumber of nodes: $nodes_cnt\n";

		print "\t\tfile: ".$file."\n";

		my $cur_labels = $ep_values->{labels};
		my $cur_nestings = $ep_values->{nesting};
		print "\t\tlabels " . basename($key) . ": ";

		# should be the most complex labels
		foreach my $label (@{$cur_labels}) {

			# Now calculate the highest evidence count we found for this label
			# AND how often this label was found
			# First some refinements to grep the correct labels
			my $refined_label = $label;
			if($label eq "DLL") {
				$refined_label = "[^C]" . $label;
			}
			$refined_label = $label . "[^C]";

=pod
			my $cur_label_count_cmd = 'grep ' . $refined_label . ' ' . $file . ' | sed -e \'s/.*>' . $label . '/' . $label . '/\' -e \'s/<\/TD><TD ALIGN="left" BGCOLOR="#[0-9A-F]*">/ /\' -e \'s/(.*//\' ';
			my $cur_label_count = `$cur_label_count_cmd`;
=cut

			# Get the evidence counts
			my $cur_count_cmd = 'grep ' . $refined_label. ' ' . $file . ' | sed -e \'s/.*>' . $label . '//\' -e \'s/<\/TD><TD ALIGN="left" BGCOLOR="#[0-9A-F]*">/ /\' -e \'s/(.*//\' ';
			my $cur_label_counts = `$cur_count_cmd`;
			my @label_counts= split(/\n/,$cur_label_counts);

			# Calculate highest evidence count and how often this label was seen
			my $highest_label_count = 0;
			my $number_labels = 0;
			for my $count (@label_counts) {
				if($highest_label_count < $count){
					$highest_label_count = $count;
				}
				$number_labels++;
			}

			# Get the strand length
			my $strand_len_count_cmd = 'grep "maxStrandLength" ' . $file . " | sed -e 's/^.*maxStrandLength://' -e 's/<.*//'";
			my $strand_len_counts = `$strand_len_count_cmd`;
			my @len_counts= split(/\n/,$strand_len_counts);

			# Calculate longest strand length
			my $highest_len_count = 0;
			for my $count (@len_counts) {
				if($highest_len_count < $count){
					$highest_len_count = $count;
				}
			}

			# Some cleanup
			$label =~ s/^\s+//;
			$label =~ s/\s+$//;
			$highest_label_count =~ s/^\s+//;
			$highest_label_count =~ s/\s+$//;
			$number_labels =~ s/^\s+//;
			$number_labels =~ s/\s+$//;
			$structs_cnt =~ s/^\s+//;
			$structs_cnt =~ s/\s+$//;
			$highest_len_count =~ s/^\s+//;
			$highest_len_count =~ s/\s+$//;

			print $label . " $highest_label_count $number_labels $structs_cnt $highest_len_count\n";

			if(!exists($clustered_hypotheses->{$label})){
				$clustered_hypotheses->{$label} = {};
			}

			# Try to find a suitable interval for this label and evidence count
			my $found_interval = 0;

			foreach my $interval ( keys %{ $clustered_hypotheses->{$label} } ) {
				my $divider = $interval <= $highest_label_count ? $highest_label_count : $interval;
				my $denominator = $interval <= $highest_label_count ? $interval : $highest_label_count;
				# Found an interval which is in range
				# The test for 0 is the special case for the undef (SLL) label
				if(($denominator == 0 && $divider == 0) ||
					($denominator / $divider) > 0.85){
					print "Found interval for label: " . $interval . "\n";
					# Multiple counts can fall into this label
					push(@{$clustered_hypotheses->{$label}->{$interval}}, "$hypothesis_name $highest_label_count $number_labels $structs_cnt $nodes_cnt $highest_len_count");
					$found_interval = 1;
					last;
				}
			}

			# No interval found, create a new one
			if(!$found_interval) {
				$clustered_hypotheses->{$label}->{$highest_label_count} = [];
				push(@{$clustered_hypotheses->{$label}->{$highest_label_count}}, "$hypothesis_name $highest_label_count $number_labels $structs_cnt $nodes_cnt $highest_len_count");
			}

			# Currently stop after first round
			last;
		}

		print "\n";

	}
}

print "*********************** Print clustered label intervals\n";
foreach my $label ( keys %$clustered_hypotheses ){
	print "Printing clusters for label: " . $label . "\n";
	foreach my $interval ( keys %{ $clustered_hypotheses->{$label} }){
		print "\tinterval: " . $interval. "\n";
		print "\t\tevidence-count label-count struct-count node-count strand-length\n";
		foreach my $found_interpretation ( @{ $clustered_hypotheses->{$label}->{$interval} } ){
			print "\t\tinterpretation labels: " . $found_interpretation . "\n";
		}
	}
}

exit;
