(rule (big-shot ?person ?division)
      (and
        (job ?person (?divison . ?x))
        (and
          (supervisor ?person ?supervisor)
          (not (job ?supervisor (?division . ?y))))))
