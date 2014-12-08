#include "test_macro.h"
#include <stdio.h>
#include "test1.h"

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

int main()
{
  struct rsa_st* p_rsa_st = calloc(sizeof(struct rsa_st), 0);
  struct evp_pkey_st* p_evp_pkey_st = calloc(sizeof(struct evp_pkey_st), 0);
  CERT_PKEY* p_cert_pkey = calloc(sizeof(CERT_PKEY), 0);
  CERT* p_cert = calloc(sizeof(CERT), 0);

  SSL* p_ssl = calloc(sizeof(SSL), 0);

  p_evp_pkey_st->pkey.rsa = p_rsa_st;
  p_cert_pkey->privateKey = p_evp_pkey_st;

  p_cert->pkey = p_cert_pkey;

  p_rsa_st = p_cert->pkey->privateKey->pkey.rsa; 

}
