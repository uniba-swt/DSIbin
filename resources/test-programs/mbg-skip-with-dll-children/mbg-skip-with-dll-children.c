#include <stdlib.h>
#include <stdio.h>

////#include "../../cil-inst/inst_util.h"

/*
 * Mbg creation test: Skip list with DLL children
 */


struct skip_linkage {
	struct skip_linkage *next;
	struct skip_linkage *down;
};

struct dll_node {
	struct dll_node *next;
	struct dll_node *prev;
};

struct skip_node {
	struct dll_node *payload;
	// Intrusive linkage
	struct skip_node *next;
	struct skip_node *down;
};

struct skip_node* create_lvl(int lvl_length) {
	struct skip_node *tmp1,*tmp2;
	struct skip_node *lvl = NULL;
	tmp1 = tmp2 = NULL;
	int i;

	if(lvl_length >= 1) {
		lvl = malloc(sizeof(*lvl));

		if(!lvl) exit(1);

		tmp1 = lvl;
		for(i = 1; i<lvl_length; i++) {
			tmp2 = malloc(sizeof(*tmp2));
			if(!tmp2) exit(1);
			tmp1->next = tmp2;
			tmp1 = tmp2;
		}
	}	
	return lvl;
}

#define CONN_LV1_TO_LV2_LEN 2
int conn_lvl1_to_lvl2[CONN_LV1_TO_LV2_LEN] = {0,2};
#define CONN_LV2_TO_LV3_LEN 3
int conn_lvl2_to_lvl3[CONN_LV2_TO_LV3_LEN] = {0,2,3};

struct skip_node* forward_to_item(struct skip_node*lvl, int item) {
	struct skip_node *iter = lvl;
	int i;
	for(i = 0; i < item; i++) {
		iter = iter->next;
	}
	return iter;
}
void conn_two_lvls(struct skip_node *first, struct skip_node *second, int *conns, int length) {
	struct skip_node *sk_link_first = first;
	struct skip_node *sk_link_second = NULL;
	int i;
	for(i = 0; i < length; i++) {
		printf("\tConnecting first index %d to second index %d\n", i, conns[i]);
		sk_link_second = forward_to_item(second,conns[i]);
		printf("\tforwarding done\n");
		sk_link_first->down = sk_link_second;
		sk_link_first = sk_link_first->next;
	}
}

void create_dll(struct dll_node *first, int items) {
	struct dll_node *tmp;
	int i;
	for(i = 0; i<items; i++)  {
		tmp = malloc(sizeof(*tmp));
		first->next = tmp;
		tmp->prev = first;
		tmp->next = NULL;
		first = tmp;
	}
}


int main(int argc, char **argv) {    

	struct skip_node *sk_ep;
	struct skip_node *lvl,*tmp;

	printf("Create first level with length %d\n", 2);
	sk_ep = create_lvl(2);
	printf("Create second level with length %d\n", 3);
	lvl = create_lvl(3);
	printf("Connect level one and two\n");
	conn_two_lvls(sk_ep,lvl,conn_lvl1_to_lvl2, CONN_LV1_TO_LV2_LEN);
	printf("Create third level with length %d\n", 4);
	lvl = create_lvl(4);
	printf("Connect level two and three\n");
	conn_two_lvls(sk_ep->down,lvl,conn_lvl2_to_lvl3, CONN_LV2_TO_LV3_LEN);

	printf("Create first payload\n");
	tmp = lvl;
	tmp->payload = malloc(sizeof(*tmp->payload));
	create_dll(tmp->payload, 2);
	// Do payload stuff here
	printf("Create second payload\n");
	tmp = tmp->next;
	tmp->payload = malloc(sizeof(*tmp->payload));
	create_dll(tmp->payload, 1);
	// Do payload stuff here
	printf("Create third payload\n");
	tmp = tmp->next;
	tmp = tmp->next;
	tmp->payload = malloc(sizeof(*tmp->payload));
	create_dll(tmp->payload, 1);
	
	lvl = NULL;

	return 0;

}

