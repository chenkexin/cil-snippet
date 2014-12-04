#include "test_func_pointer.h"

static int my_printn(int n)
{
	printf("I am %d\n", n);
	return 0;
}

static int my_printme(pkey *pk)
{
	if (!pk)
    printf("Uninitialized pkey\n");
  else
    printf("a=%d, b=%c, c=%s, len=%lu\n", pk->a, pk->b, pk->c, pk->clen);
  return 0;
}

static int my_set_pkey_a(pkey *pk, int n)
{
  if (!pk)
    return 0;
  pk->a = n;
  return 1;	
}

static int my_set_pkey_b(pkey *pk, char c)
{
  if (!pk)
    return 0;
  pk->b = c;
  return 1;	
}

static int my_set_pkey_c(pkey *pk, char *buf, ulong len)
{
	if (!pk)
    return 0;
  if (pk->clen < len) 
  {
    //secure page here
    pk->c = (char *)malloc(len);
  } else
    memset(pk->c, 0, pk->clen);
  memcpy(pk->c, buf, len);
  pk->clen = len;
  return 1;
}


static ssl_method my_ssl_method = {
	"ZTY's method `~0.0~`",
	my_printn,
	my_printme,
	my_set_pkey_a,
	my_set_pkey_b,
	my_set_pkey_c
};

static ssl_method *pkey_get_default_method(void)
{
	return &my_ssl_method;
}

pkey *pkey_new(void)
{
	pkey *ret;
	ret = malloc(sizeof(pkey));
	ret->meth = pkey_get_default_method();
	return ret;
}

int main()
{
	pkey *pk;
	pk = pkey_new();
	printf("%s\n", pk->meth->name);
	pk->meth->printn(4);
	pk->meth->set_pkey_a(pk, 2);
	pk->meth->set_pkey_b(pk, '2');
	pk->meth->set_pkey_c(pk, "hahaha", 6);
	pk->meth->printme(pk);
}