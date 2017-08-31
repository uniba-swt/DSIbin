#include <stdlib.h>
#include <stdio.h>

typedef struct _link_node {
	int payload;
	struct _link_node *next;
} link_node;	

typedef struct _barrier_node {
	int payload_barrier;
	link_node list;
} barrier_node;

int main(int argc, char **argv) {    

	int i;
	printf("Creating list\n");
	barrier_node *head = malloc(sizeof(barrier_node));
	head->list.payload = 0;

	link_node *iter = &(head->list);	
	for(i = 1; i < 10; i++){
		printf("Insert node\n");
		link_node *new = malloc(sizeof(link_node));
		new->next = NULL;
		new->payload = i;
		iter->next = new;
		iter = iter->next;
	}

	printf("Printing all nodes\n");
	iter = &(head->list);	
	while(iter){
		printf("node: %d\n", iter->payload);
		iter = iter->next;
	}

	// Leak

	return 0;
}

