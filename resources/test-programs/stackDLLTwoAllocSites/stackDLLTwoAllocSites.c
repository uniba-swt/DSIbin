#include <stdio.h>
#include <stdlib.h>


// Stack DLL example with print in normal and reverse direction
// The usage of two functions causes Howard to detect two different allocation sites
// Author: Jan H. Boockmann
// Date: 31 May 2017


struct node{
	int id;
	struct node* next;
	struct node* prev;
};

void addNodeToListA(int id, struct node* prevNode);
void addNodeToListB(int id, struct node* prevNode);


// add a new stack node to the list if id != 0
// traverse the list in normal and reverse direction if id != 0
void addNodeToListA(int id, struct node* prevNode) {
	printf("addNodeToListA\n");
	if(id == 0) {
		// stop adding new nodes. print the list and exit
		printf("Printing list in reverse: ");
		while(prevNode->prev != NULL){
			printf("%d, ",prevNode->id);
			prevNode = prevNode->prev;
		}

		printf("\nFirst item in list: %d\n", prevNode->id);

		printf("Printing list in normal: ");
		while(prevNode != NULL){
			printf("%d, ",prevNode->id);
			prevNode = prevNode->next;
		}
	} else {
		// add a new node to the list
		struct node newNode;
		newNode.id = id;
		newNode.next = NULL;
		newNode.prev = prevNode;
		if(prevNode != NULL) {
			prevNode->next = &newNode;
		}
		addNodeToListB(id-1, &newNode);
	}
}

// add a new stack node to the list if id != 0
// traverse the list in normal and reverse direction if id != 0
void addNodeToListB(int id, struct node* prevNode) {
	printf("addNodeToListB\n");
	if(id == 0) {
		// stop adding new nodes. print the list and exit
		printf("Printing list in reverse: ");
		while(prevNode->prev != NULL){
			printf("%d, ",prevNode->id);
			prevNode = prevNode->prev;
		}

		printf("\nFirst item in list: %d\n", prevNode->id);

		printf("Printing list in normal: ");
		while(prevNode != NULL){
			printf("%d, ",prevNode->id);
			prevNode = prevNode->next;
		}
	} else {
		// add a new node to the list
		struct node newNode;
		newNode.id = id;
		newNode.next = NULL;
		newNode.prev = prevNode;
		if(prevNode != NULL) {
			prevNode->next = &newNode;
		}
		addNodeToListA(id-1, &newNode);
	}
}


int main(void) {
	addNodeToListA(10, NULL);
	return 0;
}

