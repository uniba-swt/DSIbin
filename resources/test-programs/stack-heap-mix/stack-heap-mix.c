#include <stdlib.h>
#include <stdio.h>

typedef struct _link_node {
	int payload;
	struct _link_node *next;
} link_node;	

void create_list(link_node *head) {
	printf("create_list: entered\n");
	int i,no_elems = 4;
	for(i = 0; i < no_elems; i++) {
		link_node *tmp = head->next;
		head->next = malloc(sizeof(link_node));
		head->next->payload = i;
		head->next->next = tmp;
	}
}

void print_list(link_node *head){
	while(head != NULL){
		printf("print_list: element %p - payload: %d\n", head, head->payload);
		head = head->next;
	}
}

int main(int argc, char **argv) {    

	link_node static_head;
	static_head.payload = -4711;

	// Create list 
	create_list(&static_head);

	print_list(&static_head);

	return 0;
}

