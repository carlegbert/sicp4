#lang sicp

(define (require c)
  (if (not c) (amb)))

(define (list-amb l)
  (if (null? l)
    (amb)
    (amb (car l) (list-amb (cdr l)))))

(define articles '(article the a))
(define nouns '(noun student professor cat class fish))
(define verbs '(verb studies lectures eats sleeps))
(define prepositions '(prep for to in by with))
(define adjectives '(adj green big smart meretritious unctuous))
(define adverbs '(adverb quickly poorly well doggedly))

(define (generate-word word-list)
  (let ((generated-word (list-amb (cdr word-list))))
    (list (car word-list) generated-word)))

(define (generate-adjectives)
  (define (maybe-extend adjective)
    (amb adjective
         (maybe-extend (list 'adjectives
                             adjective
                             (generate-adjectives)))))
  (maybe-extend (generate-word adjectives)))

(define (generate-simple-noun-phrase)
  (let ((article (generate-word articles)))
    (amb (list 'simple-noun-phrase
               article
               (generate-word nouns))
         (list 'simple-noun-phrase
               article
               (generate-adjectives)
               (generate-word nouns)))))

(define (generate-prepositional-phrase)
  (list 'prep-phrase
        (generate-word prepositions)
        (generate-noun-phrase)))

(define (generate-verb-phrase)
  (define (maybe-extend verb-phrase)
    (let ((verb-phrase (maybe-extend-with-adverb verb-phrase)))
      (amb verb-phrase
           (maybe-extend (list 'verb-phrase
                               verb-phrase
                               (generate-prepositional-phrase))))))
  (define (maybe-extend-with-adverb verb-phrase)
    (amb verb-phrase
         (list 'verb-phrase
               verb-phrase
               (generate-word adverbs))))
  (maybe-extend (generate-word verbs)))

(define (generate-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (generate-prepositional-phrase)))))
  (maybe-extend (generate-simple-noun-phrase)))

(define (generate-sentence)
  (list 'sentence
        (generate-noun-phrase)
        (generate-verb-phrase)))


(generate-sentence)
