(rule (can-replace ?person-1 ?person-2)
      (and
        (not (same ?person-1 ?person-2))
        (job ?person-1 ?job-1)
        (job ?person-2 ?job-2)
        (or
          (same ?job-1 ?job-2)
          (can-do-job ?person-1 ?job-2))))

(can-replace (Cy D. Fect) ?replacement)

(and
  (salary ?person-1 ?salary-1)
  (salary ?person-2 ?salary-2)
  (lisp-value < ?salary-1 ?salary-2)
  (can-replace ?person-1 ?person-2))
