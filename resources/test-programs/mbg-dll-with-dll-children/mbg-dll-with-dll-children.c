#include <stdlib.h>
#include <stdio.h>

////#include "../../cil-inst/inst_util.h"

/*
 * Mbg creation test: dll with 
 * dll as child nodes
*/

struct Child_DLL {
	int payload;
	struct Child_DLL *next;
	struct Child_DLL *prev;
};


struct DLL {
	struct DLL *next;
	struct DLL *prev;
	struct Child_DLL child;
};

int main(int argc, char **argv) {    

	struct DLL *head;
	struct DLL *iter;
	int i;

	head = malloc(sizeof(*head));
	head->next = NULL;
	head->prev = NULL;

	iter = head;
	for(i = 0; i<3; i++) {
		iter->next = malloc(sizeof(*iter));
		iter->next->prev = iter;
		iter = iter->next;
		iter->next = NULL;
	}

	for(iter = head; iter->next != NULL; iter = iter->next) {
		iter->child.next = malloc(sizeof(iter->child));
		iter->child.next->prev = &iter->child;
		iter->child.next->next = malloc(sizeof(iter->child));
		iter->child.next->next->prev = iter->child.next;
		iter->child.next->next->next = NULL;
		//iter = iter->next	
	}
}

