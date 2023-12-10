(in-package :xtdb-cl)

(defparameter *q1*
  `(-> (from
        :lineitem
        #(l-shipdate
          l-quantity
          l-extendedprice
          l-discount
          l-tax
          l-returnflag
          l-linestatus))
       (where (<= l-shipdate ,(lt:parse-timestring "1998-09-02")))
       (aggregate
        l-returnflag
        l-linestatus
        ,(dict
          :sum-qty '(sum l-quantity)
          :sum-base-price '(sum l-extendedprice)
          :sum-disc-price '(sum (* l-extendedprice (- 1 l-discount)))
          :sum-charge '(sum (* (* l-extendedprice (- 1 l-discount)) (+ 1 l-tax)))
          :avg-qty '(avg l-quantity)
          :avg-price '(avg l-extendedprice)
          :avg-disc '(avg l-discount)
          :count-order '(row-count)))
       (order-by l-returnflag l-linestatus)))

(defparameter *q2*
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
       (limit 100)))


;; TODO don't know how to {:args {:segment "BUILDING}}
;; (defparameter *q3*
;;   `(-> (unify
;;         (from :customer (vect (dict :xt/id 'c :c-mktsegment 'segment)))
;;         (from :orders (vect (dict :xt/id 'o :o-custkey 'c) 'o-shippriority 'o-orderdate))
;;         (where (< o-orderdate ,(lt:parse-timestring "1995-03-15")))
;;         (from :lineitem (vect (dict  :l-orderkey 'o) 'l-discount 'l-extendedprice 'l-shipdate))
;;         (where (> l-shipdate ,(lt:parse-timestring "1995-03-15"))))
;;        (aggregate
;;         (dict  :l-orderkey o)
;;         (dict  :revenue '(sum (* l-extendedprice (- 1 l-discount))))
;;         o-orderdate
;;         o-shippriority)
;;        (order-by (dict :val 'revenue :dir :desc) o-orderdate)
;;        (limit 10)))

(defparameter *q4*
  `(-> (from :orders ,(vect (dict :xt/id 'o) 'o-orderdate 'o-orderpriority))
       (where (>= o-orderdate ,(lt:parse-timestring "1993-07-01"))
        (< o-orderdate ,(lt:parse-timestring "1993-10-01"))

        (exists? (-> (from :lineitem ,(vect (dict :l-orderkey '$o) 'l-commitdate 'l-receiptdate))
                     (where (< l-commitdate l-receiptdate)))
                 ,(dict :args (vect 'o))))

       (aggregate o-orderpriority ,(dict :order-count '(count o)))
       (order-by o-orderpriority)))

;;
;; (defparameter *q7*
;;   `(-> (unify (from :orders  ,(vect (dict :o-custkey 'c)))
;;               (from :lineitem ,(vect (dict :l-orderkey 'o :l-suppkey 's)))

;;               (where (>= l-shipdate ,(lt:parse-timestring "1995-01-01"))
;;                      (<= l-shipdate ,(lt:parse-timestring "1996-12-31")))

;;               (from :supplier ,(vect (dict :xt/id 's :s-nationkey 'n1)))
;;               (from :nation ,(vect (dict :xt/id 'n1 :n-name 'supp-nation)))
;;               (from :customer ,(vect (dict :xt/id 'c :c-nationkey 'n2)))
;;               (from :nation ,(vect (dict :xt/id 'n2 :n-name 'cust-nation)))

;;               (where (or (and (= "FRANCE" supp-nation)
;;                               (= "GERMANY" cust-nation))
;;                          (and (= "GERMANY" supp-nation)
;;                               (= "FRANCE" cust-nation)))))

;;        (with ,(dict  :l-year '(extract "YEAR" l-shipdate)))

;;        (aggregate supp-nation cust-nation l-year
;;                   ,(dict  :revenue '(sum (* l-extendedprice (- 1 l-discount)))))

;;        (order-by supp-nation cust-nation l-year)))

(defparameter *q9*
  `(-> (unify (from :part ,(vect (dict :xt/id 'p) 'p-name))
              (where (like p-name "%green%"))

              (from :orders ,(vect (dict :xt/id 'o) 'o-orderdate))

              (from :lineitem ,(vect (dict :l-orderkey 'o :l-suppkey 's :l-partkey 'p)
                                     'l-quantity 'l-extendedprice 'l-discount))

              (from :partsupp ,(vect (dict :ps-partkey 'p :ps-suppkey 's) 'ps-supplycost))

              (from :supplier ,(vect (dict :xt/id 's :s-nationkey 'n)))
              (from :nation ,(vect (dict :xt/id 'n :n-name 'nation))))

       (with ,(dict :o-year '(extract "YEAR" o-orderdate)))

       (aggregate nation o-year
                  ,(dict :sum-profit '(sum (- (* l-extendedprice (- 1 l-discount))
                                               (* ps-supplycost l-quantity)))))))

;; (defparameter *q22*
;;   `(-> (from :customer #(c-phone c-acctbal))
;;        (where (in? (subs c-phone 0 2) ,(fset:set "13" "31" "23" "29" "30" "18" "17"))

;;         (> c-acctbal
;;            (q (-> (from :customer #(c-acctbal c-phone))
;;                   (where (> c-acctbal 0.0)
;;                    (in? (subs c-phone 0 2) ,(fset:set "13" "31" "23" "29" "30" "18" "17")))
;;                   (aggregate ,(dict :avg-acctbal '(avg c-acctbal))))))

;;         (not (in? c (q (from :orders ,(vect (dict :o-custkey 'c)))))))

;;        (aggregate cntrycode
;;                   ,(dict :numcust '(count c))
;;                   ,(dict :totacctbal '(sum c-acctbal)))

;;        (order-by cntrycode)))
