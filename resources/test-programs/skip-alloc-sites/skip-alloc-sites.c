#include <stdlib.h>
#include <stdio.h>

typedef struct _link_node {
	struct _link_node *next;
	struct _link_node *down;
	int payload;
} link_node;	

link_node * create_list(link_node **head) {
	printf("create_list: entered\n");
	int i,no_elems = 4;
	link_node *tail;
	tail = malloc(sizeof(*tail));
	*head = tail;
	for(i = 0; i < no_elems; i++) {
		tail->next = malloc(sizeof(*tail));
		tail = tail->next;
	}
	return tail;
}

link_node * create_list2(link_node **head) {
	printf("create_list2: entered\n");
	int i,no_elems = 10;
	link_node *noise, *noise2, *noise3, *tail;
	tail = malloc(sizeof(*tail));
	*head = tail;
	for(i = 0; i < no_elems; i++) {
		tail->next = malloc(sizeof(*tail));
		tail->payload = i;
		tail = tail->next;
	}
	return tail;
}

void print_list(link_node *head){
	while(head != NULL){
		printf("print_list: element %p\n", head);
		head = head->next;
	}
}

int main(int argc, char **argv) {    

	link_node *list1_head, *list1_tail, *list2_head, *list2_tail, *list3_head, *list3_tail;

	// Create two head nodes
	link_node *list_head_01 = malloc(sizeof(link_node));
	link_node *list_head_02 = malloc(sizeof(link_node));
	
	// Create list for first head
	list1_tail = create_list(&list1_head);
	list_head_01->next = list1_head;

	// Create list for second head
	list2_tail = create_list(&list2_head);
	list_head_02->next = list2_head;

	// Connect two lists
	list2_tail->next = list_head_01;

	return 0;
}

