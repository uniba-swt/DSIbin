#include <stdlib.h>
#include <stdio.h>

////#include "../../cil-inst/inst_util.h"

struct parent {
	struct parent *next;
	struct parent *child;
};

void create_parent(struct parent **start, int size) {
	int i;
	printf("create_parent: size: %d\n", size);
	for(i = 0; i < size; i++) {
		*start = malloc(sizeof(**start));
		if(*start == NULL){
			printf("Error: Not enough memory. Exiting\n");
			exit(1);
		}
		(*start)->next = NULL;
		start = &(*start)->next;
	}
}

void create_child(struct parent **start, int size) {
	int i;
	printf("create_child: size: %d\n", size);
	for(i = 0; i < size; i++) {
		*start = malloc(sizeof(**start));
		if(*start == NULL){
			printf("Error: Not enough memory. Exiting\n");
			exit(1);
		}
		(*start)->child = NULL;
		start = &(*start)->child;
	}
}

int main(int argc, char **argv) {    


	struct parent *head, *iter;
	int i;
	int parents_len = 7;
	int childs_len = 2;

	create_parent(&head, parents_len);

	
	iter = head;
	for(i = 0; i<parents_len; i++) {
		create_child(&iter->child, childs_len);
		iter = iter->next;
	}
		
	return 0;
}

