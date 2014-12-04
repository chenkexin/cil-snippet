#include "test_macro.h"
#include <stdio.h>

#define bn_check_top(a) \
  do { \
    if( a != NULL) {\
      printf("%d\n", a); } \
    else a = NULL; \
  } while(0)

int* BN_copy(int*a , const int* b)
{
  int i;

#if 1
  BN_free(a);
#else
  i = BN_new();
#endif
  bn_check_top(a);
}

void BN_free(int *t)
 {
   free(t);
 }

int* BN_new()
{
  return malloc(sizeof(int));
}
