#include "s.h"
#include "r.h"
#include "sparc.h"

/* XXX is not dynamic */

/* Will actually raise table size to next higher power of 2 */
HashTable_t*
CreateHashTable(int size)
{
	HashTable_t *res = (HashTable_t *)malloc(sizeof(HashTable_t));
	int i = 0;

	while (size>0)
	{
		size/=2; 
		i++;
	}
	size = 1 << i;

	res->logsize = i;
	res->logmask = size - 1;
	res->size = size;
	res->table = (HashEntry_t *)malloc(size*sizeof(HashEntry_t));
	for (i=0; i<size; i++)
		res->table[i].key = 0xffffffff;
	return res;
}

void
DestroyHashTable(HashTable_t* h)
{
	free(h->table);
	free(h);
}

void
HashTableInsert(HashTable_t* h, HashEntry_t* e)
{
	HashEntry_t *slot = HashTableLookup(h,e->key,1);

	if (slot->key != 0xffffffff)
	{
		printf("Slot already used\n");
		exit(-1);
	}
	slot->key = e->key;
	slot->data = e->data;
}

HashEntry_t*
HashTableLookup(HashTable_t* h, unsigned long key, int insert)
{
	/*
		delta and h->size must be relatively prime; if we assume
		h->size is a power of 2, it is sufficient to make delta odd
	*/
	unsigned int b = 0x58d2d93f;
	unsigned int keyb = key * b;
	unsigned int start = keyb & h->logmask;
	unsigned int delta = ((keyb >> 8) | 1) & h->logmask;
	unsigned int cur = start;
	int count=0;
	while (1) {
		count++;
		/* printf("key = %x  cur = %xd\n", key, cur); */
		if (h->table[cur].key == key) /* found it */
			return &(h->table[cur]);
		if (h->table[cur].key == 0xffffffff) { /* uninitialized */
			if (insert)
				return &(h->table[cur]);
			return NULL;
		}
		cur = (cur + delta) & h->logmask;
		if (cur == start) {
			printf("hash table size is %d\n",h->size);
			printf("key, start, delta, count = %lu %d %d %d\n",key,
				start,delta,count);
			printf("FATAL ERROR in hash lookup: table full\n");
			exit(-1);
		}
	}
	return NULL;
}
