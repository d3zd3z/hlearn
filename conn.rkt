#lang racket

;;; The shared database connection used.  Keeps things simpler.

(require db)

(provide conn
         with-database
         get-kind
         query1)

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
        (check-schema)
        (action)))
    (lambda ()
      (disconnect db))))

;;; Check the schema version.
(define (check-schema)
  (define ver (query1 "SELECT * FROM schema_version"))
  (unless (string=? ver "20170709A")
    (error "Database schema version mismatch" ver "20170709A")))

;;; Get the problem set kind.
(define (get-kind)
  (query1 "SELECT value FROM config WHERE key = 'kind'"))

;;; A query that expects a single row of a single value.
(define (query1 query . args)
  (match (apply query-row (conn) query args)
    [(vector result) result]
    [else (error "Invalid SQL result")]))
