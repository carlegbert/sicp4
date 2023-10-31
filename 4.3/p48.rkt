#lang sicp

(define (require c)
  (if (not c) (amb)))

(define *unparsed* '())

(define articles '(article the a))
(define nouns '(noun student professor cat class fish))
(define verbs '(verb studies lectures eats sleeps))
(define prepositions '(prep for to in by with))
(define adjectives '(adj green big smart meretritious unctuous))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-adjectives)
  (define (maybe-extend adjective)
    (amb adjective
         (maybe-extend (list 'adjectives
                             adjective
                             (parse-adjectives)))))
  (maybe-extend (parse-word adjectives)))

(define (parse-simple-noun-phrase)
  (let ((article (parse-word articles)))
    (amb (list 'simple-noun-phrase
               article
               (parse-word nouns))
         (list 'simple-noun-phrase
               article
               (parse-adjectives)
               (parse-word nouns)))))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(parse '(the cat sleeps))
(parse '(the green cat sleeps))
(parse '(the big green cat sleeps))
(parse '(the big green cat eats with the smart student))
