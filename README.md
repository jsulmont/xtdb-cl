# xtdb-cl

Common Lisp client for [XTDB 2.x](https://xtdb.com/v2). 


## About XTDB 2.x

Developped by [Juxt](https://www.juxt.pro/):

 * Mostly Clojure (runs on the JVM).
 * Based on [Apache Arrow ](https://arrow.apache.org/)
 * [HTAP](https://en.wikipedia.org/wiki/Hybrid_transactional/analytical_processing)
 * Native support for both SQL-2011 and XQTL (inspired from Datalog and relational algebra).
 * Accessible from over HTTP (MIME type `application/transit+json`
 * Some support for Postgrew wire protocol
 
Currently the only HTTP client released is written in Clojure.
This projects open the door to non JVM based languages

# Usage

Run from sources:

    make run
    # aka sbcl --load run.lisp

choose your lisp:

    LISP=ccl make run

or build and run the binary:

```
$ make build
$ ./xtdb-cl [name]
Hello [name] from xtdb-cl
```

# Dev

Tests are defined with [Fiveam](https://common-lisp.net/project/fiveam/docs/).

Run them from the terminal with `make test`. You should see a failing test.

```bash
$ make test
Running test suite TESTMAIN
 Running test TEST1 f
 Did 1 check.
    Pass: 0 ( 0%)
    Skip: 0 ( 0%)
    Fail: 1 (100%)

 Failure Details:
 --------------------------------
 TEST1 in TESTMAIN []:

3

 evaluated to

3

 which is not

=

 to

2

Makefile:15: recipe for target 'test' failed

$ echo $?
2
```

On Slime, load the test package and run `run!`.

---

Licence: MIT
