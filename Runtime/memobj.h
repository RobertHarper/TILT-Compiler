#ifndef _memobj_h
#define _memobj_h

#include "tag.h"

struct StackChainObj__t;

struct StackObj__t
{
  int id;
  int valid;
  value_t top;
  value_t bottom;
  value_t rawtop;
  value_t rawbottom;
  value_t used_bottom;
  long safety;
  struct StackChainObj__t *parent;
};

typedef struct StackObj__t StackObj_t;

void StackInitialize();
StackObj_t* StackObj_Alloc();
StackObj_t* GetStack();
StackObj_t* GetRawStack();


struct StackChainObj__t
{
  int size;
  int count;
  StackObj_t **stacks;
};

typedef struct StackChainObj__t StackChainObj_t;
StackChainObj_t* StackChainObj_Alloc();
void memobj_init();


struct HeapObj__t
{
  int id;
  int valid;
  value_t top;
  value_t bottom;
  value_t rawtop;
  value_t rawbottom;
  value_t alloc_start;
  long safety;
};

typedef struct HeapObj__t HeapObj_t;

void HeapInitialize();
HeapObj_t* HeapObj_Alloc(int MinSize, int MaxSize);
HeapObj_t* GetHeap();
void HeapObj_Protect(HeapObj_t*);
void HeapObj_Unprotect(HeapObj_t*);
void HeapObj_Resize(HeapObj_t *res, long newsize);
int HeapObj_Getsize(HeapObj_t *res);

extern value_t LowHeapLimit;

#endif

