#!/usr/bin/perl

# Note: Please adjust the following variables to your respective setting: my_pin_path, my_dsibin_path, my_eclipse_path . Additionally, adjust the path to the DSI-logger project when setting the dsi_program_stub.
# In case of an error, you might have to adjust the my_eclipse_path manually. In order to do so, execute DSIbin using eclipse and copy the shell command using the debug view.


use warnings;
use strict;
use threads;
# See http://perldoc.perl.org/Thread/Queue.html
use Thread::Queue;
use File::Basename;

# Enable/disable the execution of the actual commands. Used e.g. for debugging/print only runs
my $run_system_cmds     = 1;
# Flag to turn on refinement, e.g., useful to rerun the evaluation without rerunning the refinement
my $run_refinement      = 1;
# Flag to stop after refinement
my $run_refinement_only = 0;
# Flag to stop after the computation of the baseline
my $run_baseline_only = 0;

# Number of worker threads
my $NUM_THREADS = 4;

# Set the path to Pin
my $my_pin_path = "/home/user/pin-3.2-81205-gcc-linux/";

# Set the path to DSI-bin
my $my_dsibin_path = "/home/user/Research/DSI-bin/";

# Set the path to eclipse
my $my_eclipse_path = "/home/user/Downloads/eclipse/";

# Set the java execution command (can be taken from eclipse's debug-view)
my $dsi_program_stub = "/usr/lib/jvm/java-7-openjdk-amd64/bin/java -Dfile.encoding=UTF-8 -Xbootclasspath/p:" . $my_eclipse_path . "plugins/org.scala-lang.scala-library_2.11.0.v20140415-163722-cac6383e66.jar:" . $my_eclipse_path . "plugins/org.scala-lang.scala-reflect_2.11.0.v20140415-163722-cac6383e66.jar:" . $my_eclipse_path . "plugins/org.scala-lang.scala-actors_2.11.0.v20140415-163722-cac6383e66.jar:" . $my_eclipse_path . "configuration/org.eclipse.osgi/bundles/282/1/.cp/target/lib/scala-swing.jar -classpath " . $my_dsibin_path . "bin:/home/user/Research/DSI-logger/bin:" . $my_dsibin_path . "resources/libs/graph-core_2.11-1.9.0.jar:" . $my_dsibin_path . "resources/libs/graph-dot_2.11-1.9.0.jar:" . $my_dsibin_path . "resources/libs/scala-xml_2.11-1.0.2.jar main.DsOli";

# Sanity check of command line
my $num_args = $#ARGV + 1;
if ($num_args != 4) {
	print "Usage: type-refinement.pl <absolute-path-to-trace> <absolute-path-to-xsd> <absolute-path-to-pin-tool> <name-of-binary-file>\n";
	exit;
}

# Global producer/consumer queue
my $file_list = Thread::Queue->new();    # A new empty queue

my $dsi_trace_path=$ARGV[0];
my $dsi_xsd_path=$ARGV[1];
my $dsi_oc_pin_tool_path=$ARGV[2];
my $dsi_oc_binary_file = $ARGV[3];

$dsi_trace_path =~ s/\/?$/\//;
$dsi_xsd_path =~ s/\/?$/\//;
$dsi_oc_pin_tool_path =~ s/\/?$/\//;

my $dsi_oc_pin_tool = $dsi_oc_pin_tool_path . "malloctrace.so";

my $dsi_refined_types_folder="dsi-refined-types";
my $dsi_refined_types_path=$dsi_trace_path . $dsi_refined_types_folder . "/";

print "Trace path: " . $dsi_trace_path . "\n";
print "XSD   path: " . $dsi_xsd_path . "\n";
print "Refined types path: " . $dsi_refined_types_path . "\n";
print "DSI PIN Tool path: " . $dsi_oc_pin_tool_path . "\n";
print "DSI PIN Tool: " . $dsi_oc_pin_tool . "\n";
print "Binary file: " . $dsi_oc_binary_file . "\n";

my $dsi_param_trace="--xml:" . $dsi_trace_path . "trace.xml";
my $dsi_param_type="--typexml:" . $dsi_trace_path . "types.xml";
my $dsi_param_tracexsd="--xsd:" . $dsi_xsd_path . "trace-schema.xsd";
my $dsi_param_featuretracexsd="--featuretracexsd:" . $dsi_xsd_path . "feature-trace-schema.xsd";
my $dsi_param_refine="--refine";

print "Trace parameter      : " . $dsi_param_trace . "\n";
print "Type  parameter      : " . $dsi_param_type . "\n";
print "XSD   parameter      : " . $dsi_param_tracexsd . "\n";
print "Feature XSD parameter: " . $dsi_param_featuretracexsd . "\n";

my $dsi_program = $dsi_program_stub . " " . $dsi_param_trace . " " . $dsi_param_type . " " . $dsi_param_tracexsd . " " . $dsi_param_featuretracexsd;

my $dsi_program_refine=$dsi_program . " " . $dsi_param_refine . " > /dev/null 2>&1";
print "DSI command       :" . $dsi_program . "\n";
print "DSI command refine:" . $dsi_program_refine . "\n";

my $measure_performance_cmd="/usr/bin/time -v -a -o";
my $measure_performance_file_suffix=".msrprf";
print "Measuring resources command:" . $measure_performance_cmd. "\n";


main();

sub interpreteRefinedType {
	my $thread_id = shift;


	print("Thread with id: " . $thread_id . " started. Exec enabled (0=no,1=yes): " . $run_system_cmds . "\n");
	# Thread will loop until no more work is available
	while (defined(my $file = $file_list->dequeue())) {
		print("Thread(" . $thread_id . "): processing " . $file . "\n");
		# Create the appropriate file names
		my $file_wo_ext = $file =~ s/.taint//r;
		my $heap_file = $file;
		my $stack_file = $file =~ s/taint/stack/r;
		# Create the file names for the original mergeinfo file and the newly
		# created mergeinfo file specific for the type we are looking at
		my $cleaned_trace_path = $dsi_trace_path =~ s/\/$//r;
		print "Thread(" . $thread_id . "):". "cleaned_trace_path: " . $cleaned_trace_path. "\n";
		my $project_name = substr($cleaned_trace_path, rindex($cleaned_trace_path, '/') + 1);
		print "Thread(" . $thread_id . "):". "project_name: " . $project_name. "\n";
		my $mergeinfo_file = $file =~ s/taint/mergeinfo/r;
		my $mergeinfo_file_orig = $dsi_trace_path . $project_name . ".mergeinfo";
		my $measure_performance_file = $file_wo_ext . $measure_performance_file_suffix;
		my $measure_performance_cmd_local=$measure_performance_cmd . " " . $measure_performance_file . " ";


		print "Thread(" . $thread_id . "):". "File w/o ext: " . $file_wo_ext . "\n";
		print "Thread(" . $thread_id . "):". "Heap file: " . $heap_file . "\n";
		print "Thread(" . $thread_id . "):". "Stack file: " . $stack_file . "\n";
		print "Thread(" . $thread_id . "):". "Merge info file: " . $mergeinfo_file . "\n";
		print "Thread(" . $thread_id . "):". "Merge info file orig: " . $mergeinfo_file_orig . "\n";
		print "Thread(" . $thread_id . "):". "Measure performance file:" . $measure_performance_file . "\n";
		print "Thread(" . $thread_id . "):". "Measure performance cmd: " . $measure_performance_cmd_local. "\n";

		if(1 == 1){


			# Copy the mergeinfo file: original to specific for this refined type
			my $cp_cmd = "cp " . $mergeinfo_file_orig . " " . $mergeinfo_file;
			print "Thread(" . $thread_id . "):". "cp cmd: " . $cp_cmd . "\n";
			if($run_system_cmds) {
				system($cp_cmd);
			}


			my $cmdline_param = 2;
			if($dsi_oc_binary_file eq "treeadd"){
				print "Thread(" . $thread_id . "): processing treeadd!\n";
				$cmdline_param = 4;
			}
			print "Thread(" . $thread_id . "): command line parameter: $cmdline_param!\n";

			# Re-run DSI-OC: produces new trace/types.xml with the refined types
			my $dsi_oc_cmd = $measure_performance_cmd_local .  " sudo " . " \$PIN_ROOT/pin -t " . $dsi_oc_pin_tool .
			" -o " . $dsi_trace_path . "pin-trace.out " .
			" -i " . $file_wo_ext .
			" -w " . $file_wo_ext .
			" -c " . $dsi_trace_path . $dsi_oc_binary_file . ".callstack" .
			# binary-trees-debian requires these parameters:
			# " -- " . $dsi_trace_path . $dsi_oc_binary_file . " 2 1 > /dev/null 2>&1";
			# treeadd requires these parameters:
			# " -- " . $dsi_trace_path . $dsi_oc_binary_file . " 4 1 > /dev/null 2>&1";
			" -- " . $dsi_trace_path . $dsi_oc_binary_file . " " .$cmdline_param. " 1 > /dev/null 2>&1";
			print "Thread(" . $thread_id . "):". $dsi_oc_cmd . "\n";
			if($run_system_cmds) {
				system($dsi_oc_cmd);
			}

			#exit(1);

			my $clean_arrays_cmd="sed -i 's/\\[[0-9]\\+\\]//' " . $file_wo_ext . "trace.xml";
			print "Thread(" . $thread_id . "): Array cleanup: " . $clean_arrays_cmd . "\n";
			if($run_system_cmds) {
				system($clean_arrays_cmd);
			}

			# Re-run DSI on each trace: produces named DSs
			my $dsi_param_eval_trace="--xml:" . $file_wo_ext . "trace.xml";
			my $dsi_param_eval_type="--typexml:" . $file_wo_ext . "types.xml";
			my $dsi_param_eval_tracexsd="--xsd:" . $dsi_xsd_path . "trace-schema.xsd";
			my $dsi_param_eval_featuretracexsd="--featuretracexsd:" . $dsi_xsd_path . "feature-trace-schema.xsd";
			my $dsi_param_eval_proj_name="--logfolderaddition:" . basename($file_wo_ext);

			my $dsi_eval_program = $measure_performance_cmd_local . " " . $dsi_program_stub . " " . $dsi_param_eval_trace . " " . $dsi_param_eval_type . " " . $dsi_param_eval_tracexsd . " " . $dsi_param_eval_featuretracexsd . " " . $dsi_param_eval_proj_name . " > /dev/null 2>&1";
			print "Thread(" . $thread_id . "):". "DSI command: " . $dsi_eval_program . "\n";
			if($run_system_cmds) {
				system($dsi_eval_program);
			}
		}
	}

}

sub main {
	# Optional: Execute DSI-OC: produces trace/types.xml

	# Clean trace
	my $clean_arrays_cmd="sed -i 's/\\[[0-9]\\+\\]//' " . $dsi_trace_path . "trace.xml";
	print "Main: Array cleanup: " . $clean_arrays_cmd . "\n";
	if($run_system_cmds) {
		system($clean_arrays_cmd);
	}

	# Be sure that the path for storing the refined types exists
	my $mk_dir_cmd="mkdir " . $dsi_refined_types_path;
	if($run_system_cmds){
		system($mk_dir_cmd);
	}

	my $measure_performance_file = $dsi_refined_types_path . "refinement`date +%Y-%m-%d%H%M`" . $measure_performance_file_suffix;
	my $measure_performance_cmd_local = $measure_performance_cmd . " " . $measure_performance_file;
	# Execute DSI with trace/types.xml: produces refined types
	print "DSI refinement command: " . $measure_performance_cmd_local . " " . $dsi_program_refine. "\n";
	my $dsi_exit = 0;
	if($run_system_cmds && $run_refinement) {
		system($measure_performance_cmd_local . " " . $dsi_program_refine);
		$dsi_exit = $?;
	}
	print "DSI return value: " . $dsi_exit . "\n";
	if($dsi_exit != 0){
		print "DSI refinement failed! Exiting.\n";
		exit(1);
	}

	# Refinement is done, shall we continue?
	print "Refinement done.";
	if ($run_refinement_only == 1) {
		print " Exiting.\n";
		exit(1);
	} else{
		print " Running DSI-OC.\n";
	}

	# Also run DSI on the original trace/types.xml to create the base-line from Howard
	# to compare the refinements against
	$measure_performance_file = $dsi_refined_types_path . "baseline`date +%Y-%m-%d%H%M`" . $measure_performance_file_suffix;
	$measure_performance_cmd_local = $measure_performance_cmd . " " . $measure_performance_file;
	print "DSI baseline command: " . $measure_performance_cmd_local . " " . $dsi_program . " > /dev/null 2>&1\n";
	if($run_system_cmds) {
		system($measure_performance_cmd_local . " " . $dsi_program . " > /dev/null 2>&1");
	}
	my $dsi_exit = $?;
	print "DSI baseline return value: " . $dsi_exit . "\n";
	if($dsi_exit != 0){
		print "DSI baseline failed! Exiting.\n";
		exit(1);
	}
	
	# Baseline computation is done, shall we continue?
	print "Baseline computation is done.";
	if ($run_baseline_only == 1) {
		print " Exiting.\n";
		exit(1);
	} else{
		print " Proceeed with computing the refined types.\n";
	}
	
	# Save the baseline in the dsi-refined-types folder as well
	my $mv_baseline_log_cmd = 'mv ' . $dsi_trace_path . '/log ' . $dsi_refined_types_path . '/logtypes_baseline';
	print $mv_baseline_log_cmd . "\n";
	if($run_system_cmds) {
		system($mv_baseline_log_cmd);
	}

	# Re-run DSI-OC with each refined type: produces trace/types.xml for each refined type
	print "Searching files: " . $dsi_refined_types_path . "*.taint" . "\n";
	my @files = glob($dsi_refined_types_path . "*.taint");
	my $export_pin_path_cmd = "PIN_ROOT=" . $my_pin_path . "; export PIN_ROOT;";
	print $export_pin_path_cmd . "\n";
	system($export_pin_path_cmd);

	# Enqueue work for the worker threads, which are created later on
	foreach my $file (@files){
		$file_list->enqueue($file);
	}

	# Signal that there is no more work to be sent
	$file_list->end();

	# Create the actual worker threads
	for (my $i = 0; $i < $NUM_THREADS; $i++) {
		threads->create(\&interpreteRefinedType, $i);
	}

	# Wait for all threads to finish
	$_->join() for threads->list();

	# Collect the resulting interpretations and choose the best
}
