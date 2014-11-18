/* Fake C library for CIL testing.
 * $Id: test1.h,v 1.1 2007/07/07 10:52:16 rjones Exp $
 */

#ifndef __TEST1_H__
#define __TEST1_H__

extern int test1_add (int a, int b);
extern int test1_inc (int a);
extern int test1_print (int a);
extern const char *test1_last_error (void);
extern void test1_print_last_error (void);

struct rsa_st
{
  int* n;
  int* p;
  int* q;
};

struct evp_pkey_st
{
  union
  {
    struct rsa_st *rsa;
  } pkey;
};

typedef struct evp_pkey_st EVP_PKEY;

typedef struct cert_pkey_st
{
  EVP_PKEY* privateKey;
}CERT_PKEY;

typedef struct cert_st
{
  CERT_PKEY* pkey;
  int valid;
}CERT;

typedef struct ssl_st
{
  struct cert_st* cert;
  int* tmp_pointer;
}SSL;

#endif /* __TEST1_H__ */
