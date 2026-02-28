(defpackage :pl-theorem-prover
  (:use :cl)
  (:export #:traverse
           #:well-formed-formula-p

           #:node
           #:pprint-node
           #:pprint-tableaux
           #:semantic-tableau-prover
           #:*prover*
           #:prove
           #:prove*))
