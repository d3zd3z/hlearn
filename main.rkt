#lang racket

(require "midi.rkt"
         "conn.rkt"
         "learn.rkt"
         "scales.rkt"
         "stats.rkt"
         levenshtein)

(define (run dbname)
  (with-database
    dbname
    (lambda ()
      (with-midi-in
        (lambda ()
          (ask-problems))))))

(define (ask-problems)
  (match (next-learning 2)
    [(list)
     (display "\nNo more problems to learn\n")]
    [(and all-next (list-rest prob _))
     (show-prompt all-next)
     (display "Play exercise: ")
     (flush-output)
     (match (read-exercise)
       [(list (list _))
        ;; Single note played exits.
        (display "exiting...\n")]
       [played
         (define answer (decode-exercise (exercise-answer prob)))
         (define distance (any-octave-compare played answer))
         (if (positive? distance)
           (begin
             (printf "You made ~a errors~%" distance)
             (printf "~a\n~a\n" played answer)
             (update prob 1)
             (play-correct answer))
           (begin
             (printf "Correct~%")
             (update prob 4)
             (ask-problems)))])]))

(define (play-correct answer)
  (display "Play correctly: ")
  (flush-output)
  (match (read-exercise)
    [(list (list _))
     (display "exiting...\n")]
    [played
      (define distance (any-octave-compare played answer))
      (if (zero? distance)
        (begin
          (printf "Correct~%")
          (ask-problems))
        (begin
          (printf "You still made ~a errors~%" distance)
          (play-correct answer)))]))

;;; Compare, trying the answer adjusted across several octives.
(define (any-octave-compare a b)
  (define distances
    (for/list ([oct (in-range -6 7)])
      (levenshtein a (octave-adjust b oct))))
  (foldl (lambda (a b) (if b (min a b) a)) #f distances))

(define (octave-adjust exercise oct)
  (map (lambda (notes)
         (map (lambda (note) (+ note (* 12 oct))) notes))
       exercise))

;;; Prompt for the first problem.
(define (show-prompt problems)
  (newline)
  (print-stats (problem-interval (car problems)))
  (newline)
  (for ([prob problems]
        [header (in-sequences (in-value "Play: ") (in-cycle (in-value "      ")))])
    (printf "~a~a\n" header (exercise-question prob)))
  (newline))

(module+ main
  (match (current-command-line-arguments)
    [(vector dbname)
     (run dbname)]
    [_
      (fprintf (current-error-port) "Usage: rlearn file.db\n")
      (exit 1)]))
