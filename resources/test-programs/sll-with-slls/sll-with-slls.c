#include <stdlib.h>
#include <stdio.h>

//#include "../../cil-inst/inst_util.h"

struct child {
	int payload;
	struct child *next;
};

struct parent {
	struct parent *next;
	struct child child;
};

void create_parent(struct parent **start, int size) {
	int i;
	for(i = 0; i < size; i++) {
		*start = malloc(sizeof(**start));
		if(start == NULL){
			printf("Error: Not enough memory. Exiting\n");
			exit(1);
		}
		(*start)->next = NULL;
		start = &(*start)->next;
	}
}

void create_child(struct child **start, int size) {
	int i;
	for(i = 0; i < size; i++) {
		*start = malloc(sizeof(**start));
		if(start == NULL){
			printf("Error: Not enough memory. Exiting\n");
			exit(1);
		}
		(*start)->next = NULL;
		start = &(*start)->next;
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
		create_child(&iter->child.next, childs_len);
		iter = iter->next;
	}
		
	return 0;
}

