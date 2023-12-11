(in-package :xtdb-cl)

;;(serapeum:toggle-pretty-print-hash-table)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defparameter *headers*
  '(("Content-Type" . "application/transit+json")
    ("Accept" . "application/transit+json")))

(defun spit (filename string &key (if-exists :supersede))
  (with-open-file (stream filename
                          :direction :output
                          :if-exists if-exists)
    (write-string string stream)))

(defun slurp (file-path)
  (with-output-to-string (s)
    (with-open-file (stream file-path)
      (loop for line = (read-line stream nil)
            while line do (write-line line s)))))

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

(defun merge-hash-tables! (ht &rest hts)
  "Merge all HTS into HT. Modifies HT in place."
  (mapc (lambda (next)
          (maphash (lambda (key value)
                     (setf (gethash key ht) value))
                   next))
        hts)
  ht)

;; "A class representing a client for  XTDB 2x. It stores the base and
;;  derived URLs (transactions, queries, and status). The class also
;;  keeps track of the latest submitted tx (by this client) and the
;;  thread that owns this instance."
(defclass xtdb-http-client ()
  ((url :initarg :url :reader url)
   (tx-url :accessor tx-url :initform nil)
   (query-url :accessor query-url :initform nil)
   (status-url :accessor status-url :initform nil)
   (latest-submitted-tx :accessor latest-submitted-tx :initform nil)
   (owner :accessor owner :initform (bt:current-thread))))

(defmethod initialize-instance :after ((client xtdb-http-client) &key)
  (with-slots (url tx-url query-url status-url http-stream) client
    (setf tx-url (format nil "~a/tx" url)
          query-url (format nil "~a/query" url)
          status-url (format nil "~a/status" url))))

(defmethod status ((client xtdb-http-client))
  (with-slots (status-url) client
    (multiple-value-bind (body status)
        (dex:get status-url :headers *headers*)
      (if (= 200 status)
          (clt:decode-json body)
          (error 'http-failed :status status)))))

;; TODO add Slime
(defvar *whitelisted-threads* (list "slynk-worker")
  "A list of threads that are allowed to access certain functions.")

(defun thread-whitelisted-p ()
  "Check if the current thread is in the whitelist."
  (member (bt:thread-name (bt:current-thread))
          *whitelisted-threads* :test #'string=))

(defmethod ensure-local ((client xtdb-http-client))
  (with-slots (owner) client
    (unless (or (thread-whitelisted-p)
                (eq (bt:current-thread) owner))
      (error 'not-owner-error
             :text (format nil "owner: ~a calling: ~a"
                           owner (bt:current-thread))))))

(defun make-xtdb-http-client (url)
  (make-instance 'xtdb-http-client :url url))

(define-condition not-owner-error (error)
  ((text :initarg :text :reader text)))

(define-condition xtdb-error (error)
  ((code :initarg :code :reader code)
   (reason :initarg :reason :reader reason)))

(defmethod print-object ((e xtdb-error) out)
  (with-slots (code reason) e
    (format out "#<XTDB-ERROR code: ~a reason: ~a>"
            code reason)))

(defun make-tagged-value (tag value)
  (declare (type symbol tag))
  (make-instance 'clt:tagged-value :tag tag :rep value))

(defun starting-from (tx-op from)
  (declare (type clt:tagged-value tx-op)
           (type lt:timestamp from))
  (merge-hash-tables!
   (clt::rep tx-op)
   (dict :|valid-from|
         (make-tagged-value '|time/instant| (lt:to-rfc3339-timestring from))))
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

(defmethod submit-tx ((client xtdb-http-client) tx-ops &optional (opts (dict)))
  (declare (type sequence tx-ops)
           (type hash-table opts))
  (ensure-local client)
  (let* ((content (-> (dict :|tx-ops| (coerce tx-ops 'vector)
                            :|opts| opts)
                      clt:encode-json))
         (rc (dex:post (tx-url client)
                       :content content
                       :headers *headers* )))
    (clt:decode-json rc)))

(defun edn-string (form)
  (labels ((f (acc form)
             (typecase form
               (keyword
                (setq acc (nconc acc (list
                                      (concatenate
                                       'string ":"
                                       (string-downcase (subseq (symbol-name form) 0)))))))
               (symbol
                (setq acc (nconc acc (list (string-downcase (symbol-name form))))))
               (lt:timestamp
                (setq acc (nconc acc (list (format nil "#inst \"~a\"" form)))))
               (string
                (setq acc (nconc acc (list (format nil "\"~a\"" form)))))
               (list
                (setq acc (nconc acc (list "(")))
                (dolist (item form acc)
                  (setq acc (f acc item)))
                (setq acc (nconc acc (list ")"))))
               (vector
                (setq acc (nconc acc (list "[")))
                (dotimes (i (length form) acc)
                  (setq acc (f acc (elt form i))))
                (setq acc (nconc acc (list "]"))))
               (hash-table
                (setq acc (nconc acc (list "{")))
                (maphash (lambda (key value)
                           (setq acc (f acc key))
                           (setq acc (f acc value)))
                         form)
                (setq acc (nconc acc (list "}"))))
               (t
                (setq acc (nconc acc (list (prin1-to-string form))))))))
    (let ((rc '()))
      (setq rc (f rc form))
      (reduce (lambda (a b) (concatenate 'string a " " b)) rc :initial-value ""))))

(defun decode-body (body)
  (let ((input-string
          (if (typep body 'string) body
              (flexi-streams:octets-to-string body))))
    (with-input-from-string (s input-string)
      (loop while (listen s)
            collect (clt:decode
                     (jzon:parse s :allow-multiple-content t))))))


(defun build-query (query basis basis-timeout after-tx args default-all-valid-time? default-tz)
  (let ((result (dict
                 :|query| (if (consp query)
                              (make-tagged-value '|xtdb/list| (edn-string query))
                              query))))
    (when basis
      (setf result (merge-hash-tables
                    result (dict :|basis| basis))))
    (when basis-timeout
      (setf result (merge-hash-tables
                    result (dict :|basis-timeout| basis-timeout))))
    (when after-tx
      (setf result (merge-hash-tables
                    result (dict :|after-tx| after-tx))))
    (when args
      (setf result (merge-hash-tables
                    result (dict :|args| args))))
    (when default-all-valid-time?
      (setf result (merge-hash-tables
                    result (dict :|default-all-valid-time?|
                                 default-all-valid-time?))))
    (when default-all-valid-time?
      (setf result (merge-hash-tables
                    result (dict :|default-tz| default-tz))))
    result))

(defmethod query ((client xtdb-http-client) query
                  &key basis basis-timeout after-tx args default-all-valid-time? default-tz )
  (check-type query (or cons string))
  (let* ((q (build-query query basis basis-timeout after-tx args default-all-valid-time? default-tz))
         (content (clt:encode-json q)))
    (multiple-value-bind (body status)
        (dex:post (query-url client) :content content :headers *headers*)
      (when (= 200 status)
        (decode-body body)))))

;; (let ((node (make-xtdb-http-client "http://localhost:3000")))
;;   (query node *q9*))


(defun read-args (argv)
  (let ((url "http://localhost:3000")
        (table :foobar))
    (when (plusp (length argv))
      (setf table (intern (string-left-trim ":" (first argv)) :keyword)))
    (when (> (length argv) 1)
      (setf url (cadr argv)))
    (values table url)))

(defun %main (argv)
  (multiple-value-bind (table url)
      (read-args argv)
    (let ((node (make-xtdb-http-client "http://localhost:3000"))      )
      (format t "-->url: ~a  table: ~a ~%" url table)
      (loop
        for count from 1 upto 10000
        do (let* ((xt/id (uuid:make-v4-uuid))
                  (tx-key (submit-tx
                           node
                           (-> (put table (dict :|xt/id| xt/id
                                                :|user-id| (uuid:make-v4-uuid)
                                                :|text| "yeayayaya"))
                               (vect))))
                  (rc (query node
                             `(-> (from ,table ,(vect 'xt/id 'user-id 'text))
                                  (where (= xt/id $id)))
                             :args (dict 'id  xt/id)
                             :after-tx tx-key)))
             (assert (and (= 1 (length rc))
                          (uuid:uuid= xt/id (href (car rc) :|xt/id|))))
             (sleep 0.005)
             (when (= 0 (mod count 10))
               (format t "--> count=~a~%" count)))))))

(defun main ()
  (%main (uiop:command-line-arguments)))

