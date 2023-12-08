(in-package :asdf-user)
(defsystem "xtdb-cl-tests"
  :description "Test suite for the xtdb-cl system"
  :author "Jan Sulmont <modality@pm.me>"
  :version "0.0.1"
  :depends-on (:xtdb-cl
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-xtdb-cl"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
