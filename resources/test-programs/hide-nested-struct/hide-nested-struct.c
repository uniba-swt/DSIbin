#include <stdlib.h>
#include <stdio.h>

typedef struct _link_node {
	struct _link_node *next;
	struct _link_node *prev;
	int payload;
} link_node;	

typedef struct _barrier_node {
	int payload_barrier;
	link_node list;
	link_node *barrier;
} barrier_node;

// Taken from wikipedia: https://en.wikipedia.org/wiki/Offsetof
// Effectively same as in linux kernel
#define offsetof(st, m) ((size_t)&(((st *)0)->m))
#define container_of(ptr, type, member) ({ \
		const typeof( ((type *)0)->member ) *__mptr = (ptr); \
		(type *)( (char *)__mptr - offsetof(type,member) );})

barrier_node* create_barrier_nodes(int barrier_size, int barrier_cnt) {
	int i;

	barrier_node *barriers_head = NULL;
	barrier_node *barriers_tail = NULL;

	for(i = 0; i< barrier_cnt; i++){
		if(!barriers_head){
			printf("create_barrier_nodes: created head\n");
			barriers_head = malloc(sizeof(*barriers_head));
			barriers_head->list.next = NULL;
			barriers_tail = barriers_head;
		} else {
			printf("create_barrier_nodes: created tail\n");

			barrier_node *tmp = malloc(sizeof(*tmp));
			// Terminate with NULL
			tmp->barrier = NULL;

			// Set the linkage
			barriers_tail->list.next = &(tmp->list);

			printf("barriers_tail: %p\n", barriers_tail);

			// New element has no next element
			tmp->list.next = NULL;

			// Set the new tail
			barriers_tail = tmp;
		}

		barriers_tail->payload_barrier = i*barrier_size;
		// Mark element as unused
		printf("create_barrier_nodes: setting barrier value %d\n", barriers_tail->payload_barrier);
	}
	return barriers_head;
}

void print_all_barriers(barrier_node *barriers){
	barrier_node *iter = barriers;
	while(iter != NULL){
		// Fetch the surrounding container
		if(iter->list.next){
			barrier_node *tmp = container_of(iter->list.next, barrier_node, list);
			iter = tmp;
			printf("print_all_barriers: iter: %p payload: %d\n", tmp, tmp->payload_barrier);
		} else {
			printf("iter->list.next is NULL\n");
			break;
		}
	}
}

#define BARRIER_SIZE 100
#define BARRIER_CNT  5
int main(int argc, char **argv) {    

	int list_elements[] = { 8, 16, 32, 64, 128, 256, 512, 1024, 2048 };
	int list_elements_size = 9, i;
	printf("Creating barriers\n");
	barrier_node *barriers = create_barrier_nodes(BARRIER_SIZE, BARRIER_CNT);

	printf("Printing all barrier nodes:\n");
	print_all_barriers(barriers);

	return 0;
}

