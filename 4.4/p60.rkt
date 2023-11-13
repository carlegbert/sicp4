;; Each pair shows up twice because if (lives-near person-1 person-2)
;; is valid, (lives-near person-2 person-1) will also be valid.

;; A simple way make the pairs unique might be to filter the list
;; s/t person-1 is less than person-2. Assuming we've defined a
;; person-alphabetical-lt function that works like you would expect it should:

(and (lives-near ?person-1 ?person-2)
     (lisp-value person-alphabetical-lt ?person-1 ?person-2))

(rule (lives-near ?person-1 ?person-2)
      (or
        (and
          (lisp-value person-alphabetical-lt ?person-2 ?person-1)
          (lives-near ?person-2 ?person-1))
        (and (address ?person-1 (?town . ?rest-1))
             (address ?person-2 (?town . ?rest-2))
             (not (same ?person-1 ?person-2)))))
