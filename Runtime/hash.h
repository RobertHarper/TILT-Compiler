#ifndef _hash_h
#define _hash_h

struct HashEntry
{
  unsigned long key;
  void *data;
};

typedef struct HashEntry HashEntry_t;

struct HashTable
{
  int size;
  int logsize;
  int logmask;
  HashEntry_t *table;
};

typedef struct HashTable HashTable_t;

HashTable_t *CreateHashTable(int);
void       DestroyHashTable(HashTable_t *);
HashEntry_t *HashTableLookup(HashTable_t *h, unsigned long key, int insert);
void HashTableInsert(HashTable_t *h, HashEntry_t *e);
int HashTableDelete(HashTable_t *h, unsigned long key);

#endif
