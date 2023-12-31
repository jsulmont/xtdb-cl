

"(-> (UNIFY
     (FROM :CUSTOMERS [{:XT/ID CUSTOMER-ID :NAME CUSTOMER-NAME}])
     (FROM :ORDERS [CUSTOMER-ID ORDER-VALUE]))
    (AGGREGATE CUSTOMER-ID CUSTOMER-NAME
        {:ORDER-COUNT '(ROW-COUNT)
        :TOTAL-VALUE '(SUM ORDER-VALUE)}
     (ORDER-BY {:VAL TOTAL-VALUE :DIR :DESC})
     (LIMIT 10) (WITHOUT :CUSTOMER-ID)))"
