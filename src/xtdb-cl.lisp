(in-package :xtdb-cl)

;;(serapeum:toggle-pretty-print-hash-table)

;; (declaim (optimize (speed 3) (debug 0) (safety 0)))

;; "A class representing a client for  XTDB2. It stores the base andq
;;  derived URLs (transactions, queries, and status). The class also
;;  keeps track of the latest submitted tx (by this client) and the
;;  thread that owns this instance."
(defclass xtdb-http-client ()
  ((url :initarg :url :reader url)
   (tx-url :accessor tx-url :initform nil)
   (query-url :accessor query-url :initform nil)
   (status-url :accessor status-url :initform nil)
   (latest-submitted-tx :accessor latest-submitted-tx :initform nil)
   (owner :accessor owner :initform (bt:current-thread))
   (http-stream :accessor http-stream :initform nil)))

;; (defmethod initialize-instance :after ((client xtdb-http-client) &key)
;;   (with-slots (url tx-url query-url status-url http-stream) client
;;     (setf tx-url (format nil "~a/tx" url)
;;           query-url (format nil "~a/query" url)
;;           status-url (format nil "~a/status" url)
;;           http-stream (nth-value 4 (drakma:http-request status-url :close nil)))))

(serapeum:toggle-pretty-print-hash-table)

(lt:timestamp+ (lt:now) 1  :month )

(defun read-file-as-string (file-path)
  (with-open-file (stream file-path)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun merge-hash-tables (ht &rest hts)
  "From 1 or more HTS create a single one with TEST of HT."
  (if hts
      (let ((rez (make-hash-table :test (hash-table-test ht))))
        (mapc (lambda (next)
                (maphash
                 (lambda (key value)
                   (setf (gethash key rez) value))
                 next))
              (cons ht hts))
        rez)
      ht))

(->
    (dex:get "http://localhost:3000/status")
    (clt:decode-json)

    )

(href (dict :x 1) :x)

(defun merge-hash-tables! (ht &rest hts)
  "Merge all HTS into HT. Modifies HT in place."
  (mapc (lambda (next)
          (maphash (lambda (key value)
                     (setf (gethash key ht) value))
                   next))
        hts)
  ht)

(clt:decode-json (read-file-as-string "/tmp/xx.tr"))

(defun spit (filename string &key (if-exists :supersede))
  (with-open-file (stream filename
                          :direction :output
                          :if-exists if-exists)
    (write-string string stream)))

(spit "/tmp/aa.txt"
      (-> (read-file-as-string "/tmp/q2.tr")
          (clt:decode-json)
          (href :|query|)
          (clt::rep)
          (print)
          )

      )


(defparameter *headers*
  '(("Content-Type" . "application/transit+json")
    ("Accept" . "application/transit+json")))

(defun make-tagged-value (tag value)
  (declare (type symbol tag))
  (make-instance 'clt:tagged-value :tag tag :rep value))

(defun starting-from (tx-op from)
  (declare (type clt:tagged-value tx-op)
           (type lt:timestamp from))
  (merge-hash-tables!
   (clt::rep tx-op)
   (dict :|valid-from| (make-tagged-value '|time/instant| (lt:to-rfc3339-timestring from))))
  tx-op)

(defun until (tx-op to)
  (declare (type clt:tagged-value tx-op)
           (type lt:timestamp to))
  (merge-hash-tables!
   (clt::rep tx-op)
   (dict :|valid-to| (make-tagged-value '|time/instant| (lt:to-rfc3339-timestring to))))
  tx-op)

(defun during (tx-op from to)
  (declare (type clt:tagged-value tx-op)
           (type lt:timestamp from)
           (type lt:timestamp to))
  (-> tx-op (starting-from from) (until to)))

(defun put (table doc)
  (declare (type keyword table)
           (type hash-table doc))
  (make-tagged-value '|xtdb.tx/put|
                     (dict :|table-name| table
                           :|doc| doc)))

(defparameter *tx-uri* "http://localhost:3000/tx")

(defun submit-tx (tx-ops &optional (opts (dict)))
  (declare (type sequence tx-ops)
           (type hash-table opts))
  (let* ((content (-> (dict :|tx-ops| (coerce tx-ops 'vector)
                            :|opts| opts)
                      clt:encode-json))
         (rc (dex:post *tx-uri* :content content
                                :headers *headers* )))
    (clt:decode-json rc)))

;; (let* ((now (lt:now))
;;        (tx-ops (-> (put :|lola| (dict :|xt/id| 123 :name "lola"))
;;                    (during now (lt:timestamp+ now 1 :month ))
;;                    (vect))))
;;   (submit-tx tx-ops))
