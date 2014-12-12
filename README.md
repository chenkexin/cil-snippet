cil-snippet
===========

A static analysis, which using CIL( C Intermediate Language ) framework to analysis OpenSSL library.

**ciltest1:**
----------
show how CIL works on main.c. 

**ciltest2:**
----------
analysis, the input and output of the analysis is in main.c.

**test1.c test1.h main.c:**
----------
the test environment which simulate the behaviors of OpenSSL.

**test_func_pointer.h test_func_pointer.c**
----------
the test case for abstraction of function pointers, which are widely used in OpenSSL library.

**test_macro.h test_macro.c**
----------
the test case for macros, which are widely used in BIGNUM's operation.

**Makefile:**
----------
makefile.

Usage:
==========

to see how CIL works on each line of code: 
    make run-ciltest1
to run the analysis: 
    make run-ciltest2
to run test case for function pointer:
    make run-ciltest3
to run test case for macro:
    make run-ciltest4
