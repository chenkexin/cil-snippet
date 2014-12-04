#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef unsigned long ulong;
typedef unsigned int uint;
typedef struct ssl_private_key_st pkey;
typedef struct ssl_method_st ssl_method;

struct ssl_private_key_st {
  int a;
  char b;
  char *c;  // sensitive data 
  ulong clen;
  ssl_method *meth;
};

struct ssl_method_st {
	const char *name;
	int (*printn)(int n);
	int (*printme)(pkey *pk); //be careful, no c in parameter but use c in the implementation 
	int (*set_pkey_a)(pkey* pk, int n);
	int (*set_pkey_b)(pkey *pk, char c);
	int (*set_pkey_c)(pkey* pk, char *buf, ulong len);
};