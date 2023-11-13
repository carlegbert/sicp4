(and
  (supervisor ?person (Bitdiddle Ben))
  (address ?person ?address))

(and
  (salary (Bitdiddle Ben) ?bens-salary)
  (and (salary ?person ?salary)
       (lisp-value < ?salary ?bens-salary)))

(and (supervisor ?person ?supervisor)
     (not job ?supervisor computer . ?x)
     (job ?supervisor ?job))
