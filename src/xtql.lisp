(in-package :xtql)
(use-package :parseq)

;;====== utilities
;; ok
(defrule this-kw (kw) keyword
  (:test (x) (eql x kw)))

(defun plistp (x)
  "Checks if all keys in a property list are keywords."
  (loop for (key value) on x by #'cddr
        always (keywordp key)))

(defrule plist () list
  (:test (&rest x) (plistp x))
  (:function (lambda (&rest x) (alex:plist-hash-table x))))


;; === === Expression
;;
(defrule ScalarExpr ()
    (or number
        string
        t nil))

(defrule VectorExpr ()
    (vector (* Expr))
  (:vector))

(defrule SetExpr ()
    (list (* Expr))
  )

(defrule CallExpr ()
    (and symbol (* Expr)))

(defrule Expr ()
    (or ScalarExpr
        VectorExpr
        SetExpr
        CallExpr
        ))

(trace-rule 'CallExpr :recursive t)
(trace-rule 'Expr :recursive t)
(trace-rule 'VectorExpr :recursive t)
(trace-rule 'ScalarExpr :recursive t)
(trace-rule 'SetExpr :recursive t)


(parseq 'CallExpr `(lola #(1 2 3)))

(parseq 'Expr '(#("a" 1 t #(2 3))))

(parseq 'SetExpr '((1 2 3)) :parse-error t)

(defrule xv () vector)

(parseq  'xv '((1 2)) )

;;====== Temporal
;;OK
(defrule TimestampLt () atom
  (:test (x) (typep x 'lt:timestamp)))

(defrule TimestampStr () string
  (:test (x) (lt:parse-timestring x :fail-on-error nil))
  (:function (lambda (x) (lt:parse-timestring x))))

(defrule Timestamp () (or TimestampStr TimestampLt))

(defrule TemporalFilter ()
    (or (and 'at Timestamp)
        (and 'from Timestamp)
        (and 'to Timestamp)
        (and 'in Timestamp Timestamp)
        (this-kw :all-time)))

(parseq 'TemporalFilter `(at ,(lt:now)))


;;====== BindSpec
;; OK
(defrule BindSpec ()
    (or symbol plist))

(defrule BindSpecVec ()
    (vector (+ BindSpec))
  (:vector))

(parseq 'BindSpecVec '(#(a b c))) ;; OK

(parseq 'BindSpec '(a))

(parseq 'BindSpec '(((:x 1))))

;;======FromOpts
;; OK
(defrule FromOptsMap () list
  (:test (&rest plist)
         (when (plistp plist)
           (let ((bind (getf plist :bind))
                 (valid-time (getf plist :for-valid-time ))
                 (system-time (getf plist :for-system-time)))
             (and (vectorp bind)
                  (if valid-time (typep valid-time 'lt:timestamp) t)
                  (if system-time (typep system-time 'lt:timestamp) t)))))
  (:function (lambda (&rest x) (alex:plist-hash-table x))))

(defrule FromOpts ()
    (or BindSpecVec FromOptsMap))

(parseq 'FromOpts '(#(a (:x 1 :y 12))) :parse-error t)
(parseq 'FromOpts `((:for-valid-time ,(lt:now) :bind #(a b) :for-system-time ,(lt:now))))


;;=== From
;; OK
(defrule From ()
    (and 'from keyword FromOpts))

(parseq 'From `(from :lola #(a b)))
(parseq 'From `(from :lola (:for-valid-time ,(lt:now) :bind #(a b) :for-system-time ,(lt:now))))

;;======Rel
;; OK
(defrule Rel ()
    (and 'rel Expr BindSpecVec))

(parseq 'Rel `(rel ,(+ 1 2 3) #(a)))
