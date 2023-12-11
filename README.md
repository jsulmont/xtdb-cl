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
This projects open the door to non JVM based languages clients, Rust (with a C binding) being next in line.

## API 
* Closely maps Clojure's version:
```Common Lisp
(let* ((table :|users|)
       (node (make-xtdb-http-client "http://localhost:3000"))
       (xt/id (uuid:make-v4-uuid))
       (tx-key (submit-tx
                node
                (-> (put table (dict :|xt/id| xt/id
                                     :|user-id| (uuid:make-v4-uuid)
                                     :|name| "John Doe"))
                    (vect))))
       (rc (query node
                  `(-> (from ,table ,(vect 'xt/id 'user-id 'name))
                       (where (= xt/id $id)))
                  :args (dict 'id  xt/id)
                  :after-tx tx-key)))
  (assert (and (= 1 (length rc))
               (uuid:uuid= xt/id (href (car rc) :|xt/id|))))
  rc)
```

## XTDB queries
Obviously, Common Lisp has none of Clojure syntactic suggars, and we chose not to introduce any (this is Lisp after all) but instead use some from  well established CL libs (e.g. [Serapeum](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md), [Fset](https://github.com/slburson/fset) etc).
Here is how the XQTL version of [Q2 from the TPCH suite](https://github.com/xtdb/xtdb/blob/2.x/modules/datasets/src/main/clojure/xtdb/datasets/tpch/xtql.clj#L26-L45) can be written:
``` Common Lisp
`(-> (unify (from :part ,(vect (dict :xt/id 'p) 'p-mfgr (dict  :p-size 15) 'p-type))
              (where (like p-type "%BRASS"))

              (from :partsupp ,(vect (dict :ps-partkey 'p :ps-suppkey 's) 'ps-supplycost))
              (from :supplier ,(vect  (dict  :xt/id 's :s-nationkey 'n)
                                      's-acctbal 's-address 's-name 's-phone 's-comment))
              (from :nation ,(vect (dict  :xt/id 'n :n-regionkey 'r) 'n-name))
              (from :region ,(vect (dict  :xt/id 'r :r-name "EUROPE")))

              (where (= ps-supplycost
                        (q (-> (unify (from :partsupp ,(vect (dict :ps-partkey '$p :ps-suppkey 's) 'ps-supplycost))
                                      (from :supplier ,(vect (dict :xt/id 's :s-nationkey 'n)))
                                      (from :nation ,(vect (dict :xt/id 'n :n-regionkey 'r)))
                                      (from :region ,(vect (dict :xt/id 'r :r-name "EUROPE"))))
                               (aggregate ,(dict  :min-supplycost '(min ps-supplycost))))
                           ,(dict  :args #(p))))))

       (order-by ,(dict :val 's-acctbal :dir :desc) n-name s-name p)
       (limit 100))
```
## Installation
This library is still a WIP but will be released to QuickLisp soon. 
It relies on a recent version of [cl-transit](https://github.com/jsulmont/cl-transit) not yet picked up by Quicklist. Untill then, just clone `cl-transit` in your local projects e.g.,:
``` Shell
$ git clone https://github.com/jsulmont/cl-transit ~/quicklisp/local-projects
```
For now, you can use the library from a REPL (e.g., Emacs):

* load `xtdb-cl.asd`
* compile it (e.g., `C-C C-K`)
* Fire a REPL (e.g. `C-C C-Z`)
* load `xtdb-cl` (e.g. `(ql:quickload :xtdb-cl)`


### binary
Although meant as a library, the Makefile allows to build a binary, which assuming you have an [XTDB 2.x instance running on your laptop](https://docs.xtdb.com/reference/main/installation), will loop each iteration inserting a new document, fetching it, sleep for 5ms. 
This code (and other) will move to a dedicated Package.
```
$ make build
$ ./xtdb-cl 

```

# Dev (no test yet 🤦)

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


---

Licence: MIT
