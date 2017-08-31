#include <stdlib.h>
#include <stdio.h>

typedef struct _link_node {
	struct _link_node *next;
	struct _link_node *prev;
	int payload;
} link_node;	

int no_elems = 10;

link_node * create_list(link_node **head) {
	printf("create_list: entered\n");
	int i;
	link_node *tail;
	tail = malloc(sizeof(*tail));
	*head = tail;
	(*head)->prev = NULL;
	for(i = 0; i < no_elems; i++) {
		tail->next = malloc(sizeof(*tail));
		tail->payload = i;
		tail = tail->next;
	}
	return tail;
}

link_node * create_list2(link_node **head) {
	printf("create_list2: entered\n");
	int i;
	link_node *noise, *noise2, *noise3, *tail;
	tail = malloc(sizeof(*tail));
	*head = tail;
	(*head)->prev = NULL;
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

	link_node *list1_head, *list1_tail, *list2_head, *list2_tail;
	link_node *iter;
	int i, e;

	list1_tail = create_list(&list1_head);
	list2_tail = create_list2(&list2_head);

	list1_head->prev = list2_head;

	return 0;
}

