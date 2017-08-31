#!/bin/bash

if [ $# -ne 1 ]
then
	echo "Usage: cp-howard-info.sh <name-of-test-case>";
	exit 1;
fi;


echo cp obj/howard/heap/heap.ds "$1".taint
cp obj/howard/heap/heap.ds "$1".taint
# only needed temporarily
sed -i -e 's/MD5/Function/' -e 's/struct {/struct{/' "$1".taint

echo cp obj/howard/heap/heap.callstack "$1".callstack
cp obj/howard/heap/heap.callstack "$1".callstack

echo cp obj/howard/mainEXE/stack.ds "$1".stack
cp obj/howard/mainEXE/stack.ds "$1".stack
