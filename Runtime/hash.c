#include "hash.h"
#include <stdlib.h>
#include <assert.h>

/* XXX is not dynamic */

/* will actually increase size to next highest power of 2 */
HashTable_t *CreateHashTable(int size)
{
  HashTable_t *res = (HashTable_t *)malloc(sizeof(HashTable_t));
  int i = 0;

  while (size>0)
    {size/=2; i++;}
  size = 1 << i;

  res->logsize = i;
  res->logmask = size - 1;
  res->size = size;
  res->table = (HashEntry_t *)malloc(size*sizeof(HashEntry_t));
  for (i=0; i<size; i++)
    res->table[i].key = 0xffffffff;
  return res;
}


void DestroyHashTable(HashTable_t *h)
{
  free(h->table);
  free(h);
}

int HashTableLookupRaw(HashTable_t *h, unsigned long key, HashEntry_t **res)
{
  /* delta and h->size must be rel prime; assuming h->size if a power of 2
     it is sufficient to make delta odd */
  unsigned long b = 0x58d2d93f;
  unsigned long start = ((key * b) >> 32) & h->logmask;
  unsigned long delta = (((key * b) >> 16) | 1) & h->logmask;
  unsigned long cur = start;
  int count=0;
  while (1)
    {
      count++;
      if (h->table[cur].key == key)
	{
	  *res = &(h->table[cur]);
	  return 1;
	}
      if (h->table[cur].key == 0xffffffff)
	{
	  *res = &(h->table[cur]);
	  return 0;
	}
      cur = (cur + delta) & h->logmask;
      if (cur == start)
	{
	  printf("hash table size is %d\n",h->size);
	  printf("key, start, delta, count = %d %d %d %d\n",key,start,delta,count);
	  printf("FATAL ERROR in hash lookup: table full\n");
	  exit(-1);
	}
    }
  return 0;
}

HashEntry_t *HashTableLookup(HashTable_t *h, unsigned long key)
{
  HashEntry_t *dummy;
  if (HashTableLookupRaw(h,key,&dummy))
    return dummy;
  return NULL;
}


void HashTableInsert(HashTable_t *h, HashEntry_t *e)
{
  HashEntry_t *slot = NULL;

  if (HashTableLookupRaw(h,e->key,&slot))
    {
      printf("Slot already used\n");
      exit(-1);
    }
  slot->key = e->key;
  slot->data = e->data;
}

int HashTableDelete(HashTable_t *h, unsigned long key)
{
  printf("HashTableDelete not imped\n");
  exit(-1);
}

