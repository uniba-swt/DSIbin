#include <stdlib.h>
#include <stdio.h>

typedef struct _link_node {
	struct _link_node *next;
	struct _link_node *down;
	int payload;
} link_node;	

#define put_link_node(prev, cur) \
	link_node * cur = malloc(sizeof(link_node)); \
	prev->next = cur;

int main(int argc, char **argv) {    
	
	link_node * n1 = malloc(sizeof(link_node));
	put_link_node(n1,n2);
	put_link_node(n2,n3);
	put_link_node(n3,n4);
	put_link_node(n4,n5);
	put_link_node(n5,n6);

	return 0;
}

