#include "bitmap.h"
#include <stdio.h>
#include <stdlib.h>

main()
{
  int sz = 1000;
  Bitmap_t *res = CreateBitmap(sz);
  int a,b,c,d,e;

  printf("alloced at %d\n",a=AllocRange(res,100));
  printf("alloced at %d\n",b=AllocRange(res,200));
  printf("alloced at %d\n",c=AllocRange(res,300));
  
  DeallocRange(res,a,100);
  printf("freeing a=%d\n",a);
  DeallocRange(res,b,200);
  printf("freeing b=%d\n",b);

  printf("alloced at %d\n",d=AllocRange(res,350));
  printf("alloced at %d\n",e=AllocRange(res,300));

  DestroyBitmap(res);
}
