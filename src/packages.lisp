(defpackage :xtdb-cl
  (:use :cl)
  (:nicknames #:xt)
  (:import-from :serapeum :vect :dict :href :@)
  (:import-from :local-time )
  (:import-from :arrow-macros
   :-> :->> :some-> :some->>
   :as-> :cond-> :cond->>
                :-<> :-<>> :some-<>
   :some-<>> :<> :<!>)
  (:local-nicknames
   (#:alex #:alexandria)
   (#:jzon #:com.inuoe.jzon)
   (#:lt #:local-time))
  (:export :main))

;; (defpackage :xtql
;;   (:use :cl :parseq)
;;   (:import-from :serapeum :vect :dict :href :@)
;;   (:local-nicknames
;;    (#:alex #:alexandria)
;;    (#:lt #:local-time)))
