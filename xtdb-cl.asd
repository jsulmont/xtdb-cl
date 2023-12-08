(in-package :asdf-user)

(defsystem :xtdb-cl
  :name "xtdb-cl"
  :author "Jan Sulmont <modality@pm.me>"
  :version "0.0.1"
  :license "MIT"
  :description "CL client for XTDB v2"
  :homepage "https://github/jsulmont/xtdb-cl"
  :bug-tracker ""
  :source-control (:git "https://github/jsulmont/xtdb-cl")
  :depends-on (:cl-transit
               :alexandria
               :dexador
               :flexi-streams
               :serapeum
               :rutils
               :fset
               :arrow-macros
               :bordeaux-threads
               :com.inuoe.jzon
               :parseq
               :uuid)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "xtdb-cl")
                             (:file "xtql"))))
  :build-operation "program-op"
  ;;:build-pathname "xtdb-cl"
  )
