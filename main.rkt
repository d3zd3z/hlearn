#lang racket

(require "midi.rkt"
         "conn.rkt"
         "intervals.rkt"
         "learn.rkt"
         "scales.rkt"
         "stats.rkt"
         levenshtein)

(define (run dbname)
  (with-database
    dbname
    (lambda ()
      (match (get-kind)
        ["midi"
         (with-midi-in
           (lambda ()
             (ask-problems)))]
        ["listen"
         (with-midi-out
           (lambda ()
             (ask-listening-problems)))]
        [kind (error "Unknown kind" kind)]))))

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
             ;; (printf "~a\n~a\n" played answer)
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

;;; Same as 'ask-problems' above, but for listening exercises.
(define (ask-listening-problems)
  (define problems (next-learning 1))
  (let loop ([counter 0])
    (match problems
    [(list)
     (display "\nNo more problems to learn\n")]
    [(list-rest prob _)
     (newline)
     (define-values (q a) (decode-interval prob))
     (when (zero? counter)
       (print-stats (problem-interval prob)))
     (play-exercise q #:delay 0.75)
     (display "\nInterval: ")
     (flush-output)
     (define user-answer (parse-user-interval (read-line)))
     (match user-answer
       ['done (display "\nGoodbye\n")]
       ['again (loop (add1 counter))]
       [_
         (if (= user-answer (abs a))
           (begin
             (printf "Correct~%")
             (update prob 4)
             (ask-listening-problems))
           (begin
             (printf "Incorrect, should be ~a~%" (abs a))
             (update prob 1)
             (printf "Listen again: ")
             (flush-output)
             (play-exercise q #:delay 0.75)
             (printf "\nPress enter: ")
             (flush-output)
             (read-line)
             (newline)
             (ask-listening-problems)))])])))

(define (parse-user-interval text)
  (match text
    ["q" 'done]
    ["" 'again]
    ["-2" 1]
    ["2" 2]
    ["-3" 3]
    ["3" 4]
    ["4" 5]
    ["+4" 6]
    ["-5" 6]
    ["5" 7]
    ["+5" 8]
    ["-6" 8]
    ["6" 9]
    ["+6" 10]
    ["-7" 10]
    ["7" 11]
    ["8" 11]
    [text (error "unknown interval" text)]))

(module+ main
  (match (current-command-line-arguments)
    [(vector dbname)
     (run dbname)]
    [_
      (fprintf (current-error-port) "Usage: rlearn file.db\n")
      (exit 1)]))
