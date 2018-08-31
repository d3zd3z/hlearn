#lang racket

;;; Generate/manage exercises that involve listening to intervals.  For these, the "question" will
;;; be json encoding of what to play, similar to chords previously.  The answer will be an integer,
;;; giving the number of notes in the interval.

(provide
  encode-interval decode-interval
  all-intervals)

(require json
         "learn.rkt")

(define (encode-interval notes interval)
  (exercise (jsexpr->string notes)
            (jsexpr->string interval)))

;;; Decodes an interval, returning two results, the notes, and the interval.
(define (decode-interval ex)
  (values (string->jsexpr (exercise-question ex))
          (string->jsexpr (exercise-answer ex))))

;;; The range of these initial exercises is based on my poor vocal range from low F to an Eb about
;;; two octaves above that.  The core of the exercise needs nearly two octaves to be able to produce
;;; intervals up to an octave from every pitch.  However, it tries to keep the exercise interval
;;; near the middle of this range since the ends are a little uncomfortable.
;;;
;;; Once the initial intervals have been learned starting from each pitch, we then generate all of
;;; the rest valid starting notes across the keyboard.
;;;
;;; The first session produces all of the minor seconds first, but randomly, and then all of the
;;; major seconds, and so on.  After all of the intervals have been introduced this way, values
;;; across the keyboard are introduced randomly.

;;; The note we strive to center the exercises around.  The high note of the interval will always be
;;; within the octave ending at this note, not including the octave below.
(define vocal-center 57)

;;; Given a list of notes, potentially adjust that list of notes to try and center it in _my_
;;; comfortable vocal range.  It biases to being slightly below if the given note is not contained
;;; in the interval.
(define (center-notes notes)
  (define largest (foldl max -inf.0 notes))
  ;;; Move down, until the largest note is <= the vocal-center.
  (define adjust-down
    (let loop ([adjust 0]
               [largest largest])
      (if (> largest vocal-center)
        (loop (- adjust 12) (- largest 12))
        adjust)))

  ;;; Move up until the largest note is within an octave of the vocal-center.
  (define cap (- vocal-center 12))
  (define adjust
    (let loop ([adjust adjust-down]
               [largest (+ largest adjust-down)])
      (if (<= largest cap)
        (loop (+ adjust 12) (+ largest 12))
        adjust)))

  (map (lambda (n) (+ n adjust)) notes))

;;; Generate all of the exercises for a given interval.
(define (make-for-interval interval)
  (for/list ([base (range 12)])
    (center-notes (list base (+ base interval)))))

;;; Return the intervalic exercises up to an octave in a nice order to learn them in.
(define (intervals-list)
  (append-map (lambda (x) (list x (- x))) (range 1 13)))

;;; Given notes, return them as a sequential series.
(define (to-sequential notes)
  (map list notes))

(define (to-harmonic notes)
  (list notes))

;;; Generate all of the intervalic exercises centered around the desired vocal range.
;;; The encoder should be one of to-sequential, or to-harmonic defined above.  The exercises within
;;; an interval are shuffled.
(define (make-intervalic encoder)
  (append* (for/list ([iv (in-list (intervals-list))])
             (shuffle (for/list ([notes (make-for-interval iv)])
                        (encode-interval (encoder notes) iv))))))

;;; Return all of the exercises so far.
(define (all-intervals)
  (make-intervalic to-sequential))
