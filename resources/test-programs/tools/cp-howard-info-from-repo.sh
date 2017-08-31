#!/bin/bash

echo "!! Script copies the merged ds file !!"
if [ $# -ne 2 ]
then
	echo "Usage: cp-howard-info.sh <path-to-repository> <name-of-test-case>";
	exit 1;
fi;


echo cp "$1"/obj/howard/heap/merge_heap.ds "$2".taint
cp "$1"/obj/howard/heap/merge_heap.ds "$2".taint
# only needed temporarily
sed -i -e 's/MD5/Function/' -e 's/struct {/struct{/' "$2".taint

echo cp "$1"/obj/howard/heap/heap.callstack "$2".callstack
cp "$1"/obj/howard/heap/heap.callstack "$2".callstack

echo cp "$1"/obj/howard/heap/merge_heap.info "$2".mergeinfo
cp "$1"/obj/howard/heap/merge_heap.info "$2".mergeinfo

echo cp "$1"/obj/howard/mainEXE/stack.ds "$2".stack
cp "$1"/obj/howard/mainEXE/stack.ds "$2".stack

