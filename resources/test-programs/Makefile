# gstabs only for debugging purposes, not used in any way by the DSI tool chain

# The path to the malloctrace.so file from DSI-bin-inst
DSI_BIN_INST_MALLOC_TRACE = ../obj-intel64/malloctrace.so

sll-with-slls: sll-with-slls/sll-with-slls.c
	gcc sll-with-slls/sll-with-slls.c -o sll-with-slls/sll-with-slls
	gcc -S sll-with-slls/sll-with-slls.c -o sll-with-slls/sll-with-slls.s
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./sll-with-slls/pin-trace.out -- ./sll-with-slls/sll-with-slls


linux-kernel-list-howard-short: linux-kernel-list-howard-short/linux-kernel-list-howard-short.c
	gcc -gstabs linux-kernel-list-howard-short/linux-kernel-list-howard-short.c -o linux-kernel-list-howard-short/linux-kernel-list-howard-short
	gcc -g -S linux-kernel-list-howard-short/linux-kernel-list-howard-short.c -o linux-kernel-list-howard-short/linux-kernel-list-howard-short.s
	objdump -S linux-kernel-list-howard-short/linux-kernel-list-howard-short > linux-kernel-list-howard-short/linux-kernel-list-howard-short.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./linux-kernel-list-howard-short/pin-trace.out -- ./linux-kernel-list-howard-short/linux-kernel-list-howard-short


binary-tree: binary-tree/binary-tree.c
	gcc -gstabs binary-tree/binary-tree.c -o binary-tree/binary-tree
	gcc -g -S binary-tree/binary-tree.c -o binary-tree/binary-tree.s
	objdump -S binary-tree/binary-tree > binary-tree/binary-tree.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./binary-tree/pin-trace.out -- ./binary-tree/binary-tree


skip-alloc-sites: skip-alloc-sites/skip-alloc-sites.c
	gcc -gstabs skip-alloc-sites/skip-alloc-sites.c -o skip-alloc-sites/skip-alloc-sites
	gcc -g -S skip-alloc-sites/skip-alloc-sites.c -o skip-alloc-sites/skip-alloc-sites.s
	objdump -S skip-alloc-sites/skip-alloc-sites > skip-alloc-sites/skip-alloc-sites.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./skip-alloc-sites/pin-trace.out -- ./skip-alloc-sites/skip-alloc-sites


skip-alloc-sites-artifical-pointers: skip-alloc-sites-artifical-pointers/skip-alloc-sites-artificial-pointers.c
	gcc -gstabs skip-alloc-sites-artifical-pointers/skip-alloc-sites-artificial-pointers.c -o skip-alloc-sites-artifical-pointers/skip-alloc-sites-artifical-pointers
	gcc -g -S skip-alloc-sites-artifical-pointers/skip-alloc-sites-artificial-pointers.c -o skip-alloc-sites-artifical-pointers/skip-alloc-sites-artifical-pointers.s
	objdump -S skip-alloc-sites-artifical-pointers/skip-alloc-sites-artifical-pointers > skip-alloc-sites-artifical-pointers/skip-alloc-sites-artifical-pointers.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./skip-alloc-sites-artifical-pointers/pin-trace.out -- ./skip-alloc-sites-artifical-pointers/skip-alloc-sites-artifical-pointers


jonathan-skip-list: jonathan-skip-list/jonathan-skip-list.c
	gcc -gstabs jonathan-skip-list/jonathan-skip-list.c -o jonathan-skip-list/jonathan-skip-list
	gcc -g -S jonathan-skip-list/jonathan-skip-list.c -o jonathan-skip-list/jonathan-skip-list.s
	objdump -S jonathan-skip-list/jonathan-skip-list > jonathan-skip-list/jonathan-skip-list.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./jonathan-skip-list/pin-trace.out -- ./jonathan-skip-list/jonathan-skip-list


mbg-dll-with-dll-children: mbg-dll-with-dll-children/mbg-dll-with-dll-children.c
	gcc -gstabs mbg-dll-with-dll-children/mbg-dll-with-dll-children.c -o mbg-dll-with-dll-children/mbg-dll-with-dll-children
	gcc -g -S mbg-dll-with-dll-children/mbg-dll-with-dll-children.c -o mbg-dll-with-dll-children/mbg-dll-with-dll-children.s
	objdump -S mbg-dll-with-dll-children/mbg-dll-with-dll-children > mbg-dll-with-dll-children/mbg-dll-with-dll-children.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./mbg-dll-with-dll-children/pin-trace.out -- ./mbg-dll-with-dll-children/mbg-dll-with-dll-children


dll-with-two-dll-children: dll-with-two-dll-children/dll-with-two-dll-children.c
	gcc -gstabs dll-with-two-dll-children/dll-with-two-dll-children.c -o dll-with-two-dll-children/dll-with-two-dll-children
	gcc -g -S dll-with-two-dll-children/dll-with-two-dll-children.c -o dll-with-two-dll-children/dll-with-two-dll-children.s
	objdump -S dll-with-two-dll-children/dll-with-two-dll-children > dll-with-two-dll-children/dll-with-two-dll-children.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./dll-with-two-dll-children/pin-trace.out -- ./dll-with-two-dll-children/dll-with-two-dll-children


binary-trees-debian: binary-trees-debian/binary-trees-debian.c
	gcc -gstabs binary-trees-debian/binary-trees-debian.c -o binary-trees-debian/binary-trees-debian -lm
	gcc -S binary-trees-debian/binary-trees-debian.c -o binary-trees-debian/binary-trees-debian.s
	objdump -S binary-trees-debian/binary-trees-debian > binary-trees-debian/binary-trees-debian.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./binary-trees-debian/pin-trace.out -- ./binary-trees-debian/binary-trees-debian 2


five-level-sll-destroyed-top-down: five-level-sll-destroyed-top-down/five-level-sll-destroyed-top-down.c
	gcc five-level-sll-destroyed-top-down/five-level-sll-destroyed-top-down.c -o five-level-sll-destroyed-top-down/five-level-sll-destroyed-top-down
	gcc -S five-level-sll-destroyed-top-down/five-level-sll-destroyed-top-down.c -o five-level-sll-destroyed-top-down/five-level-sll-destroyed-top-down.s
	objdump -S five-level-sll-destroyed-top-down/five-level-sll-destroyed-top-down > five-level-sll-destroyed-top-down/five-level-sll-destroyed-top-down.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./five-level-sll-destroyed-top-down/pin-trace.out -- ./five-level-sll-destroyed-top-down/five-level-sll-destroyed-top-down


mbg-skip-with-dll-children: mbg-skip-with-dll-children/mbg-skip-with-dll-children.c
	gcc mbg-skip-with-dll-children/mbg-skip-with-dll-children.c -o mbg-skip-with-dll-children/mbg-skip-with-dll-children
	gcc -S mbg-skip-with-dll-children/mbg-skip-with-dll-children.c -o mbg-skip-with-dll-children/mbg-skip-with-dll-children.s
	objdump -S mbg-skip-with-dll-children/mbg-skip-with-dll-children > mbg-skip-with-dll-children/mbg-skip-with-dll-children.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./mbg-skip-with-dll-children/pin-trace.out -- ./mbg-skip-with-dll-children/mbg-skip-with-dll-children


sll-with-two-dlls: sll-with-two-dlls/sll-with-two-dlls.c
	gcc sll-with-two-dlls/sll-with-two-dlls.c -o sll-with-two-dlls/sll-with-two-dlls
	gcc -S sll-with-two-dlls/sll-with-two-dlls.c -o sll-with-two-dlls/sll-with-two-dlls.s
	objdump -S sll-with-two-dlls/sll-with-two-dlls > sll-with-two-dlls/sll-with-two-dlls.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./sll-with-two-dlls/pin-trace.out -- ./sll-with-two-dlls/sll-with-two-dlls


two-dlls-direct: two-dlls-direct/two-dlls-direct.c
	gcc -gstabs two-dlls-direct/two-dlls-direct.c -o two-dlls-direct/two-dlls-direct
	#gcc two-dlls-direct/two-dlls-direct.c -o two-dlls-direct/two-dlls-direct
	gcc -S two-dlls-direct/two-dlls-direct.c -o two-dlls-direct/two-dlls-direct.s
	objdump -S two-dlls-direct/two-dlls-direct > two-dlls-direct/two-dlls-direct.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./two-dlls-direct/pin-trace.out -- ./two-dlls-direct/two-dlls-direct


weiss-sll-cut1: weiss-sll-cut1/weiss-sll-cut1.c
	gcc weiss-sll-cut1/weiss-sll-cut1.c -o weiss-sll-cut1/weiss-sll-cut1
	gcc -S weiss-sll-cut1/weiss-sll-cut1.c -o weiss-sll-cut1/weiss-sll-cut1.s
	objdump -S weiss-sll-cut1/weiss-sll-cut1 > weiss-sll-cut1/weiss-sll-cut1.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./weiss-sll-cut1/pin-trace.out -- ./weiss-sll-cut1/weiss-sll-cut1 999


weiss-stack-cut1: weiss-stack-cut1/weiss-stack-cut1.c
	gcc weiss-stack-cut1/weiss-stack-cut1.c -o weiss-stack-cut1/weiss-stack-cut1
	gcc -S weiss-stack-cut1/weiss-stack-cut1.c -o weiss-stack-cut1/weiss-stack-cut1.s
	objdump -S weiss-stack-cut1/weiss-stack-cut1 > weiss-stack-cut1/weiss-stack-cut1.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./weiss-stack-cut1/pin-trace.out -- ./weiss-stack-cut1/weiss-stack-cut1 999


wolf-dll: wolf-dll/wolf-dll.c
	gcc wolf-dll/wolf-dll.c -o wolf-dll/wolf-dll
	gcc -S wolf-dll/wolf-dll.c -o wolf-dll/wolf-dll.s
	objdump -S wolf-dll/wolf-dll > wolf-dll/wolf-dll.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./wolf-dll/pin-trace.out -- ./wolf-dll/wolf-dll 999


wolf-queue-cut1: wolf-queue-cut1/wolf-queue-cut1.c
	gcc -gstabs wolf-queue-cut1/wolf-queue-cut1.c -o wolf-queue-cut1/wolf-queue-cut1
	gcc -S wolf-queue-cut1/wolf-queue-cut1.c -o wolf-queue-cut1/wolf-queue-cut1.s
	objdump -S wolf-queue-cut1/wolf-queue-cut1 > wolf-queue-cut1/wolf-queue-cut1.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./wolf-queue-cut1/pin-trace.out -- ./wolf-queue-cut1/wolf-queue-cut1 999


vnc-rfbregion: vnc-rfbregion/vnc-rfbregion.c
	gcc -gstabs vnc-rfbregion/vnc-rfbregion.c -o vnc-rfbregion/vnc-rfbregion
	gcc -S vnc-rfbregion/vnc-rfbregion.c -o vnc-rfbregion/vnc-rfbregion.s
	objdump -S vnc-rfbregion/vnc-rfbregion > vnc-rfbregion/vnc-rfbregion.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./vnc-rfbregion/pin-trace.out -- ./vnc-rfbregion/vnc-rfbregion 999


simple-nesting: simple-nesting/simple-nesting.c
	gcc -gstabs simple-nesting/simple-nesting.c -o simple-nesting/simple-nesting
	gcc -g -S simple-nesting/simple-nesting.c -o simple-nesting/simple-nesting.s
	objdump -S simple-nesting/simple-nesting > simple-nesting/simple-nesting.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./simple-nesting/pin-trace.out -- ./simple-nesting/simple-nesting


stack-heap-mix: stack-heap-mix/stack-heap-mix.c
	gcc -gstabs stack-heap-mix/stack-heap-mix.c -o stack-heap-mix/stack-heap-mix
	gcc -g -S stack-heap-mix/stack-heap-mix.c -o stack-heap-mix/stack-heap-mix.s
	objdump -S stack-heap-mix/stack-heap-mix > stack-heap-mix/stack-heap-mix.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./stack-heap-mix/pin-trace.out -- ./stack-heap-mix/stack-heap-mix


prevent-merge-alloc-sites-different-functions: prevent-merge-alloc-sites-different-functions/prevent-merge-alloc-sites-different-functions.c
	gcc -gstabs prevent-merge-alloc-sites-different-functions/prevent-merge-alloc-sites-different-functions.c -o prevent-merge-alloc-sites-different-functions/prevent-merge-alloc-sites-different-functions
	gcc -g -S prevent-merge-alloc-sites-different-functions/prevent-merge-alloc-sites-different-functions.c -o prevent-merge-alloc-sites-different-functions/prevent-merge-alloc-sites-different-functions.s
	objdump -S prevent-merge-alloc-sites-different-functions/prevent-merge-alloc-sites-different-functions > prevent-merge-alloc-sites-different-functions/prevent-merge-alloc-sites-different-functions.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./prevent-merge-alloc-sites-different-functions/pin-trace.out -- ./prevent-merge-alloc-sites-different-functions/prevent-merge-alloc-sites-different-functions


hide-nested-struct: hide-nested-struct/hide-nested-struct.c
	gcc -gstabs hide-nested-struct/hide-nested-struct.c -o hide-nested-struct/hide-nested-struct
	gcc -g -S hide-nested-struct/hide-nested-struct.c -o hide-nested-struct/hide-nested-struct.s
	objdump -S hide-nested-struct/hide-nested-struct > hide-nested-struct/hide-nested-struct.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./hide-nested-struct/pin-trace.out -- ./hide-nested-struct/hide-nested-struct


hide-nested-struct-extended: hide-nested-struct-extended/hide-nested-struct-extended.c
	gcc -gstabs hide-nested-struct-extended/hide-nested-struct-extended.c -o hide-nested-struct-extended/hide-nested-struct-extended
	gcc -g -S hide-nested-struct-extended/hide-nested-struct-extended.c -o hide-nested-struct-extended/hide-nested-struct-extended.s
	objdump -S hide-nested-struct-extended/hide-nested-struct-extended > hide-nested-struct-extended/hide-nested-struct-extended.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./hide-nested-struct-extended/pin-trace.out -- ./hide-nested-struct-extended/hide-nested-struct-extended


sll-with-slls-same-type: sll-with-slls-same-type/sll-with-slls-same-type.c
	gcc -O0 sll-with-slls-same-type/sll-with-slls-same-type.c -o sll-with-slls-same-type/sll-with-slls-same-type
	gcc -O0 -S sll-with-slls-same-type/sll-with-slls-same-type.c
	objdump -S sll-with-slls-same-type/sll-with-slls-same-type > sll-with-slls-same-type/sll-with-slls-same-type.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./sll-with-slls-same-type/pin-trace.out -- ./sll-with-slls-same-type/sll-with-slls-same-type


treeadd: treeadd/treeadd.c
	gcc treeadd/args.c  -c -o treeadd/obj/args.o
	gcc treeadd/plain.c  -c -o treeadd/obj/plain.o
	gcc treeadd/par-alloc.c  -c -o treeadd/obj/par-alloc.o
	gcc treeadd/node.c -c -o treeadd/obj/node.o
	gcc treeadd/obj/node.o treeadd/obj/args.o treeadd/obj/par-alloc.o treeadd/obj/plain.o -o treeadd/treeadd -lm
	objdump -S treeadd/treeadd > treeadd/treeadd.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./treeadd/pin-trace.out -- ./treeadd/treeadd 4 1


custom-linked-list: custom-linked-list/custom-linked-list.cpp
	g++ -O0 -gstabs custom-linked-list/custom-linked-list.cpp -o custom-linked-list/custom-linked-list
	g++ -S custom-linked-list/custom-linked-list.cpp -o custom-linked-list/custom-linked-list.s
	objdump -S custom-linked-list/custom-linked-list > custom-linked-list/custom-linked-list.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./custom-linked-list/pin-trace.out -- ./custom-linked-list/custom-linked-list


custom-linked-list-nested: c++-custom-linked-list-nested/c++-custom-linked-list-nested.cpp
	g++ -O0 -gstabs c++-custom-linked-list-nested/c++-custom-linked-list-nested.cpp -o c++-custom-linked-list-nested/c++-custom-linked-list-nested
	g++ -S c++-custom-linked-list-nested/c++-custom-linked-list-nested.cpp -o c++-custom-linked-list-nested/c++-custom-linked-list-nested.s
	objdump -S c++-custom-linked-list-nested/c++-custom-linked-list-nested > c++-custom-linked-list-nested/c++-custom-linked-list-nested.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./c++-custom-linked-list-nested/pin-trace.out -- ./c++-custom-linked-list-nested/c++-custom-linked-list-nested


agobot: agobot/agobot.c
	g++ -O0 -gstabs agobot/agobot.c -o agobot/agobot
	g++ -S agobot/agobot.c -o agobot/agobot.s
	objdump -S agobot/agobot > agobot/agobot.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./agobot/pin-trace.out -- ./agobot/agobot


cpp-std-list-nesting: cpp-std-list-nesting/cpp-std-list-nesting.cpp
	g++ -O0 cpp-std-list-nesting/cpp-std-list-nesting.cpp -o cpp-std-list-nesting/cpp-std-list-nesting
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./cpp-std-list-nesting/pin-trace.out -- ./cpp-std-list-nesting/cpp-std-list-nesting


fast-forward-list: fast-forward-list/fast-forward-list.c
	gcc -gstabs fast-forward-list/fast-forward-list.c -o fast-forward-list/fast-forward-list
	gcc -S fast-forward-list/fast-forward-list.c -o fast-forward-list/fast-forward-list.s
	objdump -S fast-forward-list/fast-forward-list > fast-forward-list/fast-forward-list.objdump
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./fast-forward-list/pin-trace.out -- ./fast-forward-list/fast-forward-list 999


stackDLL: stackDLL/stackDLL.c
	gcc stackDLL/stackDLL.c -o stackDLL/stackDLL
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./stackDLL/pin-trace.out -- ./stackDLL/stackDLL


stackDLL: stackDLLTwoAllocSites/stackDLLTwoAllocSites.c
	gcc stackDLLTwoAllocSites/stackDLLTwoAllocSites.c -o stackDLLTwoAllocSites/stackDLLTwoAllocSites
	sudo $(PIN_ROOT)/pin -t $(DSI_BIN_INST_MALLOC_TRACE) -o ./stackDLLTwoAllocSites/pin-trace.out -- ./stackDLLTwoAllocSites/stackDLLTwoAllocSites
