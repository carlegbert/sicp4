(meeting ?department (Friday ?time))

(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?department . ?x))
           (or (meeting whole-meeting ?day-and-time)
               (meeting ?department ?day-and-time))))

(and
  (meeting-time (Alyssa P Hacker) (Wednesday ?time))
  (meeting . ?meeting-details))
