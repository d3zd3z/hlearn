#lang racket

(require db
         "conn.rkt"
         "learn.rkt"
         "scales.rkt")

;;; An augmented exercise with an ID.
(struct id-exercise exercise (id) #:transparent)

;;; Read in all of the problems, returning a list of id-exercise for them.
(define (read-problems)
  (map (lambda (row)
         (match row
           [(vector id question answer)
            (id-exercise question answer id)]
           [else (error "Invalid response from database")]))
       (query-rows (conn)
                   "SELECT id, question, answer \
                   FROM probs")))

(define (augment-exercises db-probs generated-problems)
  ;; A mapping from the text of the question to the problems themselves.
  (define db-map (for/hash ([p (in-list db-probs)])
                   (values (exercise-question p) p)))
  ;; Verify that there are no duplicates.
  (unless (= (hash-count db-map)
             (length db-probs))
    (error "Database contains duplicated problems"))

  ;; Loop through the generated problems, filtering out all those that match in the exercise.
  (define to-add
    (filter identity
            (for/list ([prob (in-list generated-problems)])
              (define db-prob (hash-ref db-map (exercise-question prob) #f))
              ;; If it exists in the db, verify that the answer is the same, else do the insert.
              (if db-prob
                (begin
                  (unless (equal? (decode-exercise (exercise-answer prob))
                                  (decode-exercise (exercise-answer db-prob)))
                    ;; Since it was present, remove it so we can tell what is getting missed.
                    (printf "Problem with different answers: ~a\n  ~a\n  ~a\n"
                            (exercise-question prob)
                            (decode-exercise (exercise-answer prob))
                            (decode-exercise (exercise-answer db-prob))))
                  (set! db-map (hash-remove db-map (exercise-question prob)))
                  #f)
                prob))))
  (printf "~a problems unaccounted for in DB~%" (hash-count db-map))
  (printf "Need to add ~a problems~%" (length to-add))
  )

(define (check filename)
  (with-database filename
                 (lambda ()
                   (augment-exercises (read-problems)
                                      (all-exercises)))))

(module+ main
  (check "scales-today.db"))
