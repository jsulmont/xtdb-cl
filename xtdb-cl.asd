(in-package :asdf-user)

(defsystem :xtdb-cl
  :name "xtdb-cl"
  :author "Jan Sulmont <dev@janks.eu>"
  :version "0.0.1"
  :license "MIT"
  :description "CL client for XTDB 2.x"
  :homepage "https://github/jsulmont/xtdb-cl"
  :bug-tracker ""
  :source-control (:git "https://github/jsulmont/xtdb-cl")
  :depends-on (:cl-transit
               :alexandria
               :dexador
               :flexi-streams
               :serapeum
               :fset
               :arrow-macros
               :bordeaux-threads
               :com.inuoe.jzon
               :uuid
               :parseq)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "xtdb")
                             (:file "tpch")
                             ;;(:file "xtql")
                             )))
  :build-operation "program-op"
  :build-pathname "xtdb-cl"
  :entry-point "xtdb:main"
  )
