#lang racket

;;; The shared database connection used.  Keeps things simpler.

(require db)

(provide conn
         with-database)

;;; The database connection.
(define conn (make-parameter #f))

;;; Perform an operation within the context of a database connection.  Note that this probably will
;;; fail if the dynamic extent is left and returned to.
(define (with-database filename action)
  (define db (sqlite3-connect #:database filename))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([conn db])
        (action)))
    (lambda ()
      (disconnect db))))

