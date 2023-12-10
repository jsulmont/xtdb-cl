(in-package :xtql)
(use-package :parseq)

(serapeum:toggle-pretty-print-hash-table)

;;====== utilities ;; ok
(defrule this-kw (kw) keyword
  (:test (x) (eql x kw)))

(defun symbol-not-keyword-p (obj)
  "Check if OBJ is a symbol but not a keyword."
  (and (symbolp obj) (not (keywordp obj))))

(defun plistp (x &optional (kind :keyword))
  "Checks if all keys in a property list are keywords."
  (loop for (key value) on x by #'cddr
        always (if (eql kind :keyword)
                   (keywordp key)
                   (symbol-not-keyword-p key))))

(defrule posintp () number
  (:test (x) (and (integerp x) (> x 0))))

(defrule plist-keyword () list
  (:test (&rest x) (plistp x))
  (:function (lambda (&rest x) (alex:plist-hash-table x))))

(defrule plist-symbol () list
  (:test (&rest x) (plistp x :symbol))
  (:function (lambda (&rest x) (alex:plist-hash-table x))))

(parseq 'plist-keyword'((a 1 b 2)))

;; === === Expression
;;
(defrule ScalarExpr () atom)

(defrule SymbolExpr () symbol)

(defrule VectorExpr ()
    (vector (* Expr))
  (:vector))

(defrule SetExpr ()
    (list (* Expr))
  (:function
   (lambda (x) (fset:convert 'fset:set x))))

;; TODO GetFieldExpr &  CallExpr
;; (defrule CallExpr ()
;;     (and SymbolExpr (* Expr)))
;; (defrule GetFieldExpr ()
;;     (and 'dot Expr SymbolExpr))



(defrule Expr ()
    (or
     ;; SubQueryExpr
     ;; ExistsExpr
     ;; PullExpr
     ;; PullManyExpr
     ;; GetFieldExpr
     ;; CallExpr
     VectorExpr
     SetExpr
     SymbolExpr
     ScalarExpr))

(parseq 'Expr `(( (dot ,(+ 1 2) lola) 1)))




(parseq 'SetExpr '((1 2 3)) :parse-error t)

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
    (or symbol plist-keyword))

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

(parseq 'Rel `(rel  ,(+ 1 2 3) #(a)))

(parseq 'Rel `(rel ,(+ 1 2) ,(vect '(:x 1 :b 2) '(:a 3 :b 4))))



;;====== Tail Operators
;;
(defrule Limit () (list 'limit posintp))

(defrule Offset () (list 'offset posintp))

(defrule OrderSpecMap () list
  (:test (&rest plist)
         (when (plistp plist)
           (let ((val (getf plist :val))
                 (dir (getf plist :dir))
                 (nulls (getf plist :nulls)))
             (and val (atom val) ;; TODO must be an atom for now :?
                  (if dir (member dir '(:asc :desc)) t)
                  (if nulls (member nulls '(:first :last)) t)))))
  (:function (lambda (&rest x) (alex:plist-hash-table x))))

(defrule OrderSpec ()
    (or symbol OrderSpecMap))

(defrule OrderBy ()
    (list 'order-by (+ OrderSpec))
  (:flatten))

(defrule ReturnSpec ()
    (or symbol plist-keyword))

(defrule Return ()
    (list 'return (+ ReturnSpec))
  (:flatten))

(defrule Where ()
    (list 'where (* Expr))
  (:flatten))

(defrule WithSpec ()
    (or symbol plist-keyword))

(defrule With ()
    (list 'with (* WithSpec))
  (:flatten))

(defrule WithSpecUnify ()
    (or symbol plist-symbol))

(defrule WithUnify ()
    (list 'with (* WithSpecUnify))
  (:flatten))

(parseq 'With '((with (:a  1))))

;; ====== Joins
;;
(defrule JoinOptsMap () list
  (:test (&rest plist)
         (when (plistp plist)
           (let ((bind (getf plist :bind))
                 (args (getf plist :args))              )
             (and (vectorp bind)
                  (if args (vectorp args) t)))))
  (:function (lambda (&rest x) (alex:plist-hash-table x))))

(defrule JoinOpts ()
    (or BindSpecVec JoinOptsMap))

(defrule Join ()
    (and 'join Query JoinOpts)
  (:flatten))

(defrule LeftJoin ()
    (and 'lef-join Query JoinOpts)
  (:flatten))

(parseq 'Join `(join ))

;; ====== Unify
;;
(defrule UnifyClause  ()
    (or From Join LeftJoin Rel Where With))

(defrule Unify ()
    (and 'unify (+ UnifyClause))
  (:flatten))

(parseq 'UnifyClause
        `(rel ,(+ 1 2) ,(vect '(:x 1 :b 2) '(:a 3 :b 4)))
        )

(parseq 'Unify
        `(unify  (rel ,(+ 1 2) ,(vect '(:x 1 :b 2) '(:a 3 :b 4)))))


;; ====== Query
;;
(defrule SourceOp ()
    (or From Rel Unify))

(defrule TailOp ()
    (or Limit Offset OrderBy Return Where With))

(parseq 'TailOp '((limit 100)))

(defrule xx ()
    (* TailOp)
  )

(defrule Query ()
    (and SourceOp (* TailOp)))

(parseq 'Query `((rel ,(+ 1 2) ,(vect '(:x 1 :b 2) '(:a 3 :b 4)))
                 (limit 100)              )
        )

(progn
  (trace-rule 'SourceOp)
  (trace-rule 'TailOp)
  (trace-rule 'Query)
  (trace-rule 'Unify)
  (trace-rule 'Rel)
  (trace-rule 'From)
  (trace-rule 'Join)
  (trace-rule 'LeftJoin)
  (trace-rule 'CallExpr :recursive t)
  (trace-rule 'SymbolExpr :recursive t)
  (trace-rule 'Expr :recursive t)
  (trace-rule 'VectorExpr :recursive t)
  (trace-rule 'ScalarExpr :recursive t)
  (trace-rule 'SetExpr :recursive t))
