#include <stdlib.h>
#include <stdio.h>

#include <string.h>

#define MAX 20

#include "list.h"

#include <time.h>

#include <string.h>

#include <sys/types.h>

#include <unistd.h>

typedef struct{
			int yy;
			int zz;
			struct list_head list;
} inner1_t;
typedef struct{
		int xx;
		inner1_t inner1;
} inner2_t;


typedef struct {
	int payload;
	inner2_t inner2;
} list_element;

int alloc_and_link(int yy, int zz, int xx, int payload, struct list_head* head){
	list_element *pitem;

	pitem = (list_element*)malloc(sizeof(list_element));
	pitem->payload = payload;
	pitem->inner2.xx = xx;
	pitem->inner2.inner1.yy = yy;
	pitem->inner2.inner1.zz = zz;
	list_add(&(pitem->inner2.inner1.list), head);
}

int main(int argc, char* argv[]) 
{

	int i;
	int payload02 = 2;
	inner1_t *pinner1;
	inner2_t *pinner2;
	list_element *pelem;
	unsigned offset;

	struct list_head *pnext;
	struct list_head phead;


	INIT_LIST_HEAD(&phead);

	for (i=0; i<10; i++){
		alloc_and_link(rand()%10,rand()%10, rand()%10, rand()%10, &phead);
	}
	
	int sum = 0;
	list_for_each(pnext, &phead){
		pinner1 = list_entry(pnext, inner1_t, list);
		pinner2 = list_entry(pinner1, inner2_t, inner1);
		pelem = list_entry(pinner2, list_element, inner2);
		offset = ((unsigned long)pnext - (unsigned long)pelem);
		printf("offset : %lx, pnext : %lx, pelem  : %lx\n",offset, pnext, pelem);
		sum += pelem->payload + pinner2->xx + pinner1->yy + pinner1->zz;
	}
	pnext = phead.next;
	while (pnext!=&phead){
		pelem = (list_element*) ((unsigned long)pnext - (unsigned long)offset);
		printf("New, offset : %lx, pnext : %lx, pelem : %lx\n", offset, pnext, pelem);
		pnext = pnext->next;
		list_del(&(pelem->inner2.inner1.list));
		free(pelem);
	}
	printf("sum %d\n", sum);
	return 0;
}
