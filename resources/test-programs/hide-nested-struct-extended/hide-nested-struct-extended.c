#include <stdlib.h>
#include <stdio.h>

typedef struct _link_node {
	int payload;
	struct _link_node *next;
} link_node;	

typedef struct _type_node {
	link_node link;
	int type_id;
} type_node;

typedef struct _barrier_node {
	int payload_barrier;
	type_node types;
} barrier_node;

typedef struct _barrier_node2 {
	int payload_barrier;
	int payload_barrier2;
	type_node types;
} barrier_node2;

// Taken from wikipedia: https://en.wikipedia.org/wiki/Offsetof
// Effectively same as in linux kernel
#define offsetof(st, m) ((size_t)&(((st *)0)->m))
#define container_of(ptr, type, member) ({ \
		const typeof( ((type *)0)->member ) *__mptr = (ptr); \
		(type *)( (char *)__mptr - offsetof(type,member) );})

void create_barrier_node(link_node *list_head, int barrier_type, int barrier_cnt) {
	int i;

	if(barrier_type == 0) {
		barrier_node *node = malloc(sizeof(barrier_node));
		// Access from head of created node
		node->payload_barrier = barrier_cnt;
		node->types.link.next = list_head->next;
		node->types.link.payload = 13;
		node->types.type_id = 0;
		list_head->next = &node->types.link;
	} else {
		barrier_node2 *node = malloc(sizeof(barrier_node2));
		// Access from head of created node
		node->payload_barrier = barrier_cnt;
		node->payload_barrier2 = barrier_cnt;
		node->types.link.next = list_head->next;
		node->types.link.payload = 37;
		node->types.type_id = 1;
		list_head->next = &node->types.link;
	}
	return;
}

void iterate_list(link_node *list_head) {
	link_node *iter = list_head->next;
	while(iter){
		int type_id;
		type_node *type_info = container_of(iter, type_node, link);	
		if(type_info->type_id == 0){
			barrier_node *node = container_of(type_info, barrier_node, types);
			printf("Do something with barrier_node: %d\n", node->payload_barrier);
			// Access from head of node -> should prevent nesting detection
			iter = node->types.link.next;
			// Access from head of link node -> should reveal nesting
			//iter = iter->next;
		}
		else {
			barrier_node2 *node = container_of(type_info, barrier_node2, types);
			printf("Do something with barrier_node2: %d, %d\n", node->payload_barrier,
					node->payload_barrier2
			      );
			// Access from head of node -> should prevent nesting detection
			//iter = node->types.link.next;
			// Access from head of link node -> should reveal nesting
			iter = iter->next;
		}
	}
}


int main(int argc, char **argv) {    

	link_node *head = malloc(sizeof(link_node));
	int i;
	head->payload = 0;
	for(i = 0; i < 10; i++){
		create_barrier_node(head, i % 2, i);
	}
	iterate_list(head);
	return 0;
}

