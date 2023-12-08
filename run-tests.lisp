
(load "xtdb-cl.asd")
(load "xtdb-cl-tests.asd")

(ql:quickload "xtdb-cl-tests")

(in-package :xtdb-cl-tests)

(uiop:quit (if (run-all-tests) 0 1))
