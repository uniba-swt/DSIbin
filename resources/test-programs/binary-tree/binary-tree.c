#include <stdlib.h>
#include <stdio.h>

////#include "../../cil-inst/inst_util.h"

struct binary_tree {
	int payload;
	struct binary_tree *left;
	struct binary_tree *right;
};

void insert(struct binary_tree *root, struct binary_tree *element) {
	if(element->payload < root->payload) {
		if(root->left != NULL) {
			insert(root->left, element);
		} else {
			root->left = element;
		}
	} else if(element->payload > root->payload) {
		if(root->right != NULL) {
			insert(root->right, element);
		} else {
			root->right = element;
		}
	}
}

void print_tree(struct binary_tree *root) {
	if(root->left != NULL) {
		printf("[l]");
		print_tree(root->left);
	}
	printf("%d\n", root->payload);
	if(root->right != NULL){
		printf("[r]");
		print_tree(root->right);
	}
}

int main(int argc, char **argv) {    

	int payload[] = {20, 10, 5, 15, 30, 25, 35, 60, 50, 45, 55, 70, 65, 75};
	int i;
	int root_payload = 40;
	int payload_len = 14;
	struct binary_tree *root;
	struct binary_tree *elem_tmp;

	root = malloc(sizeof(*root));
	root->left = root->right = NULL;
	root->payload = root_payload;

	for(i = 0; i<payload_len; i++) {
		elem_tmp = malloc(sizeof(*elem_tmp));
		elem_tmp->payload = payload[i];
		insert(root, elem_tmp);
	}

	//print_tree(root);

	/* Leak memory :) */

	return 0;
}

