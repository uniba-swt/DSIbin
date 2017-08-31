#include <stdio.h>
#include <stdlib.h>


// Stack DLL example with print in normal and reverse direction
// Author: Jan H. Boockmann
// Date: 31 May 2017


struct node{
	int id;
	struct node* next;
	struct node* prev;
};


// add a new stack node to the list if id != 0
// traverse the list in normal and reverse direction if id != 0
void addNodeToList(int id, struct node* prevNode) {
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
		addNodeToList(id-1, &newNode);
	}
}


int main(void) {
	addNodeToList(10, NULL);
	return 0;
}



