/* Main program for testing CIL.
 * $Id: main.c,v 1.1 2007/07/07 10:52:16 rjones Exp $
 */

/* modified test for openssl
 * 2014/11/20 chenkx
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test1.h"
static int* test_global = NULL;

/*
 * If notate p_rsa_st as sentitive data, then 
 * the result should be: 
 * 
 * (int i_tmp) * 
 * (int tmp)
 * (int b)
 * (int* ret)
 * (int** test_double_array
 * (int* test_ret)
 * (static int* test_global)
 * Total: 7
 */
int* test_hehe(int** i, int c, int *b)
{
  int* ret = NULL;
  ret = b;
  *i = ret;
  test_global = b;
  return ret;
};

int
main ()
{
  int** test_double_array=NULL;

  int* test_ret = NULL; 
  //int a, d;

//  int check_memcpy = 0;

  //a = 1; d = 0;
  /*a = test1_add (c, b);
  c = a + b;
  test1_print (a);

  a = b;
  b = a;
  a = test1_inc (a);
  test1_print (b);

  memcpy(&check_memcpy, &a, 4);
  d = test1_add( a, b);
  d = d - 1;
  */
  int i_tmp = 7;
  int a,b,c,d;
  a = 0; b = 1; c = 2; d = 3;
  struct rsa_st* p_rsa_st = calloc(sizeof(struct rsa_st), 0);
  struct evp_pkey_st* p_evp_pkey_st = calloc(sizeof(struct evp_pkey_st), 0);
  CERT_PKEY* p_cert_pkey = calloc(sizeof(CERT_PKEY), 0);
  CERT* p_cert = calloc(sizeof(CERT), 0);
  SSL* p_ssl = calloc(sizeof(SSL), 0);

  p_rsa_st->n = &a;
  p_rsa_st->p = &b;
  p_rsa_st->q = &c;

  p_evp_pkey_st->pkey.rsa = p_rsa_st;

  p_cert_pkey->privateKey = p_evp_pkey_st;

  p_cert->pkey = p_cert_pkey;
  p_cert->valid = 3;
 
  p_ssl->cert = p_cert;
  p_ssl->tmp_pointer = &i_tmp;  

  int tmp = *(p_ssl->cert->pkey->privateKey->pkey.rsa->n); 
  i_tmp =*(p_rsa_st->p);
  test_ret = test_hehe(test_double_array,d, p_rsa_st->p);
  printf( "%d, %08x", tmp, *test_ret); 
  exit (0);
}
