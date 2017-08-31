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
		// Init head/tail first
		if(!barriers_head){
			printf("create_barrier_nodes: created head\n");
			barriers_head = malloc(sizeof(*barriers_head));
			barriers_head->list.next = NULL;
			barriers_head->list.prev = NULL;
			barriers_tail = barriers_head;
			// Terminate with NULL
			barriers_tail->barrier = NULL;
		} else {
			printf("create_barrier_nodes: created tail\n");

			barrier_node *tmp = malloc(sizeof(*tmp));
			// Terminate with NULL
			tmp->barrier = NULL;

			// Set the barrier
			barriers_tail->barrier = &(tmp->list);

			printf("barriers_tail: %p\n", barriers_tail);
			printf("barriers_tail->barrier: %p\n", barriers_tail->barrier);

			// New element has no next element
			tmp->list.next = NULL;
			// New element prev is current tail list
			tmp->list.prev = &barriers_tail->list;
			// Current tail list next element is new elements list
			barriers_tail->list.next = &(tmp->list);

			// Set the new tail
			barriers_tail = tmp;
		}
		printf("last allocated element: %p\n", barriers_tail);
		// Always set the value for this barrier
		barriers_tail->payload_barrier = i*barrier_size;
		// Mark element as unused
		barriers_tail->list.payload = -1;
		printf("create_barrier_nodes: setting barrier value %d\n", barriers_tail->payload_barrier);
	}
	return barriers_head;
}

void insert_node(int value, barrier_node *barriers) {
	barrier_node *iter = barriers;
	// Forward to barrier element which marks the
	// beginning of this slice
	printf("insert_node: value: %d\n", value);
	while(iter && iter->barrier){

		// Fetch the surrounding container
		barrier_node *tmp = container_of(iter->barrier, barrier_node, list);

		printf("Checking next barrier: %d. Current barrier: %d.\n", 
				tmp->payload_barrier, iter->payload_barrier);

		// Stop as soon as the next barrier is bigger than our current value
		if(tmp->payload_barrier > value) {
			break;
		}
		iter = tmp;
	}

	// Insert the elment
	if(iter != NULL){
		printf("Found barrier: %d\n", iter->payload_barrier);
		// Element of barrier unused
		if(iter->list.payload == -1){
			printf("Using barrier node.\n"); 
			iter->list.payload = value;
			// Element of barrier already in use, add new elment
		} else {
			printf("Barrier node already used. Creating new.\n"); 
			link_node *new_node = malloc(sizeof(*new_node));
			new_node->next = iter->list.next;
			new_node->prev = &iter->list;
			if(new_node->next){
				new_node->next->prev = new_node;
			}
			new_node->payload = value;
			iter->list.next = new_node;
		}
	}
}

void print_all_barriers(barrier_node *barriers){
	barrier_node *iter = barriers;
	while(iter != NULL){
		printf("Barrier node: %d\n", iter->payload_barrier);

		// Emergency stop to prevent fetching a non existant container
		if(iter->barrier == NULL) break;

		// Fetch the surrounding container
		barrier_node *tmp = container_of(iter->barrier, barrier_node, list);
		iter = tmp;
		printf("print_all_barriers: iter: %p\n", tmp);
	}
}

void print_all_list_nodes(barrier_node *barriers){

	if(!barriers) return;

	link_node *iter = &barriers->list;
	while(iter){
		printf("list node: %d\n", iter->payload);
		iter = iter->next;
	}
}

#define BARRIER_SIZE 100
#define BARRIER_CNT  5
int main(int argc, char **argv) {    

	int list_elements[] = { 8, 16, 32, 64, 128, 256, 512, 1024, 2048 };
	int list_elements_size = 9, i;
	printf("Creating barriers\n");
	barrier_node *barriers = create_barrier_nodes(BARRIER_SIZE, BARRIER_CNT);

	for(i = 0; i < list_elements_size; i++){
		insert_node(list_elements[i], barriers);
	}

	printf("Printing all barrier nodes:\n");
	print_all_barriers(barriers);
	printf("Printing all link nodes:\n");
	print_all_list_nodes(barriers);

	return 0;
}

