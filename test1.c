/* Fake C library for CIL testing.
 * $Id: test1.c,v 1.2 2007/07/07 12:03:30 rjones Exp $
 */

/* modified test for openssl.
 * 2014/11/20 Chenkx
 */
#include <stdio.h>

#include "test1.h"

static const char *last_err = NULL;

int
test1_add (int a, int b)
{
  if (a >= 0 && b >= 0)
    return a+b;
  else {
    last_err = "Negative value given to test1_add";
    return -1;
  }
}

int
test1_inc (int a)
{
  if (a >= 0)
  {
    int* check_addr  = NULL;
    check_addr = &a;
    test1_print( *check_addr);
    return a+1;
    
  }
  else
    /* Here is a putative error in the library - we are returning
     * an error indication without setting the internal
     * 'last_err' flag.  Static analysis ought to be able to
     * find errors like this.
     */
    return -1;
}

int
test1_print (int a)
{
  if (a >= 0) {
    printf ("a = %d\n", a);
    return 0;
  } else {
    last_err = "Negative value given to test1_print";
    return -1;
  }
}

const char *
test1_last_error (void)
{
  const char *err = last_err;
  last_err = NULL;
  return err;
}

void
test1_print_last_error (void)
{
  const char *err = last_err;
  last_err = NULL;
  if (err) fprintf (stderr, "test1: %s\n", err);
}
