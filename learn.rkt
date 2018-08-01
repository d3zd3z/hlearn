#lang racket

;;; Learning utilities.

(require db
         "conn.rkt")

(provide (struct-out exercise)
         (struct-out problem)
         next-learning
         next-new
         update

         current-db-time)

;;; An exercise is a question and answer pair in some form interpreted by the particular problem
;;; set.
(struct exercise (question answer) #:transparent)

;;; A "problem" is an exercise that also carries an association with a database entry, and some
;;; fields from the database.
(struct problem exercise (id next interval) #:transparent)

;;; Query for up to 'n' upcoming problems that have expired.  This will return the list of problems,
;;; with an element 0 being the next problem that should be asked.  If there are no pending
;;; problems, will return a single problem.
(define (next-learning n)
  (define now (current-db-time))
  (define nexts
    (map
      (lambda (row)
        (match row
          [(vector id question answer next interval)
           (problem question answer id next interval)]
          [else (error "Invalid SQL result row")]))
      (query-rows (conn)
                  (string-append
                    "SELECT id, question, answer, next, interval"
                    " FROM probs JOIN learning"
                    " WHERE probs.id = learning.probid"
                    "   AND next <= ?"
                    " ORDER BY next"
                    " LIMIT ?")
                  now n)))
  (if (null? nexts) (next-new) nexts))

;;; If next-learning returns an empty list, next-new can be used to retrieve a single problem that
;;; hasn't been learned before.  This returns a list of 0 or 1 problems.
(define (next-new)
  (define now (current-db-time))
  (define rows
    (query-rows (conn)
               (string-append
                 "SELECT id, question, answer"
                 " FROM probs"
                 " WHERE id NOT IN (SELECT probid FROM learning)"
                 " ORDER BY id"
                 " LIMIT 1")))
  (map
    (lambda (row)
      (match row
        [(vector id question answer)
         (problem (string-append question " (NEW)") answer id now 5.0)]
        [else (error "Invalid SQL result")]))
    rows))

;;; Update a problem, based on a learning factor.  The scale is 1..4, with 1 being totally
;;; incorrect, and 4 being totally correct.
(define (update prob factor)
  (define adjust (case factor
                   [(1) 0.25]
                   [(2) 0.9]
                   [(3) 1.2]
                   [(4) 2.2]
                   [else (error "Invalid factor: " factor)]))
  (define now (current-db-time))
  (define fudge (+ (* (random) 0.5) 0.25))
  (define interval (max (* (problem-interval prob) adjust fudge) 5.0))
  (define next (+ now interval))

  (query-exec (conn)
              "INSERT OR REPLACE INTO learning VALUES (?, ?, ?)"
              (problem-id prob)
              next
              interval))

;;; Time in the database is a floating point number of seconds since 1970.
(define (current-db-time)
  (/ (current-milliseconds) 1000.0))
