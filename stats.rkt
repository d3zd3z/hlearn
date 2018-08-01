#lang racket

(require db
         "conn.rkt"
         "learn.rkt")

(provide print-stats)

;;; Gather statistics
(define (print-stats cur-interval)
  (define now (current-db-time))
  (define unlearned
    (query1 (string-append
              "SELECT COUNT(*)"
              " FROM probs"
              " WHERE id NOT IN (SELECT probid FROM learning)")))
  (define active
    (query1 (string-append
              "SELECT COUNT(*)"
              " FROM probs JOIN learning"
              " WHERE probs.id = learning.probid"
              "   AND next <= ?")
            now))
  (define later
    (query1 (string-append
              "SELECT COUNT(*)"
              " FROM probs JOIN learning"
              " WHERE probs.id = learning.probid "
              "   AND next > ?")
            now))
  (printf "Active: ~a, Later: ~a, Unlearned: ~a, Interval: ~a~%"
          active later unlearned (humanize-time cur-interval))
  (show-buckets (bucket-query)))

(define (bucket-query)
  (define interval 1.0)
  (define prior 0)
  (for/list ([limit (in-list '(60 60 24 30 1e30))]
             [name (in-list '("sec" "min" "hr" "day" "mon"))])
    (define new-interval (* interval limit))
    (define count (query1 (string-append
                            "SELECT COUNT(*)"
                            " FROM probs JOIN learning"
                            " WHERE probs.id = learning.probid"
                            "   AND interval <= ? AND interval > ?")
                          new-interval prior))
    (set! prior new-interval)
    (set! interval new-interval)
    (cons name count)))

(define (show-buckets buckets)
  (define total (foldl + 0 (map cdr buckets)))
  (for ([bucket (in-list buckets)])
    (define name (car bucket))
    (define num (cdr bucket))
    (printf "  ~a: ~a ~a~%"
            (~a name #:min-width 4)
            (~r num  #:min-width 6)
            (gen-stars num total))))

(define (gen-stars num total)
  (make-string
    (if (positive? total)
      (quotient (* num 65) total)
      0)
    #\*))

;;; Humanize a time.
(define (humanize-time seconds)
  (for/or ([name (in-list '("seconds" "minutes" "hours" "days" "months" "years"))]
             [time (in-list '(60.0 60.0 24.0 30.0 12.0 1.0e9))])
    (define answer (if (< seconds time)
                     (format "~a ~a" (~r seconds #:precision '(= 1)) name)
                     #f))
    (set! seconds (/ seconds time))
    answer))
