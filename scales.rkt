#lang racket

(provide
  encode-exercise decode-exercise
  all-exercises)

(require json
         "learn.rkt")

;;; Previous renditions of this software tried to always generate the
;;; exercises with the same indices.  Instead, we will rely on the
;;; generated names, and for those that exist, even have a way to
;;; ensure that the resulting exercise is the same.

(define (encode-exercise ex)
  (jsexpr->string
    (hasheq 'type "voicing"
             'chords ex)))

(define (decode-exercise text)
  (define js (string->jsexpr text))
  (unless (string=? (hash-ref js 'type) "voicing")
    (error "TODO: So far, we only support voicing exercises"))
  (hash-ref js 'chords))

;;; These are the starting notes for each exercise.
(define bases '("C" "G" "D" "A" "E" "B" "F♯" "G♭" "D♭" "A♭" "E♭" "B♭" "F"))

;;; Decode a note into Midi.  The octave is not considered, and will always be starting with C as
;;; Midi note value 60 (so 59 is possible with a flag sign).  Raises an error if the string is
;;; nonsensical.
(define (note-value text)
  (unless (<= 1 (string-length text) 2)
    (error "Expecting 1 or 2 characters for a MIDI note" text))
  (define base (case (string-ref text 0)
                 [(#\C) 60]
                 [(#\D) 62]
                 [(#\E) 64]
                 [(#\F) 65]
                 [(#\G) 67]
                 [(#\A) 69]
                 [(#\B) 71]
                 [else (error "Unknown note" text)]))
  (define bias
    (if (< (string-length text) 2)
      0
      (case (string-ref text 1)
        [(#\♯ #\#) 1]
        [(#\♭ #\b) -1]
        [else "Unknown alteration" text])))
  (+ base bias))

;;; Take a textual representation of the intervals in a scale and return a list of the numeric
;;; intervals.
(define (decode-intervals text)
  (for/list ([ch (in-string text)])
    (case ch
      [(#\H) 1]
      [(#\W) 2]
      [(#\m) 3]
      [(#\M) 4]
      [(#\5) 5]
      [else (error "Unsupported interval" ch)])))

;;; Build a generator for a given scale, at a given offset.  The base note is the midi value of note
;;; '0' in the scale, and the string is a sequence of interval-values (small integers).  The result
;;; is a function that takes an index, normalizes it to the proper range, and returns the note
;;; value.  Raises an error if the intervals do not result in an exact octave.
(define (interval-gen base-note intervals)
  (define octave-size (length intervals))
  (define steps (make-vector octave-size))
  (let loop ([pos 0]
             [note base-note]
             [iv intervals])
    (cond [(pair? iv)
           (vector-set! steps pos note)
           (loop (add1 pos) (+ note (car iv)) (cdr iv))]
          [else
            (unless (= note (+ base-note 12))
              (error "Given intervals do not form an octave" intervals))]))
  (lambda (index)
    (define-values (index-2 bias-2)
      (let loop ([index index]
                 [bias 0])
        (if (negative? index)
          (loop (+ index octave-size) (- bias 12))
          (values index bias))))
    (define-values (index-3 bias-3)
      (let loop ([index index-2]
                 [bias bias-2])
        (if (>= index octave-size)
          (loop (- index octave-size) (+ bias 12))
          (values index bias))))
    (+ (vector-ref steps index-3)
       bias-3)))

;;; Generate a practice pattern for 'n' notes in a scale, climbing to the top, and back down, using
;;; 'oct' octaves.
(define (make-scale #:notes notes
                    #:octaves oct
                    #:extra extra)
  (append (range 0 (* notes oct))
          (range (* notes oct)
                 (- extra)
                 -1)))

(struct style (name offsets octaves extra) #:transparent)
(define style-up-down (style "" '(0) 2 0))
(define style-3-up (style " 3rds" '(0 2) 1 2))
(define style-3-up-rev (style " 3rds rev" '(2 0) 1 2))

(define (style-notes st notes)
  (append* (map (lambda (n)
                  (map (lambda (ofs) (+ n ofs)) (style-offsets st)))
                notes)))

;;; There are two axes for the handedness of the exercises:
;;; RH - Right hand
;;; LH - Left hand
;;; 2H - Both hands together.
;;; Right and left hand only differ in the textual description (the exercises are octave adjusted to
;;; match).

;;; Generate a chord generator where each chord is based on offsets.
(define (chordify offsets)
  (lambda (notes)
    (for/list ([note (in-list notes)])
      (for/list ([offset (in-list offsets)])
        (+ note offset)))))

(struct handedness (names chord-gen))

(define hands-both-progressive
  (handedness '("RH" "LH" "2H")
              (list (chordify '(0))
                    (chordify '(0))
                    (chordify '(0 12)))))
(define hands-both
  (handedness '("2H") (list (chordify '(0 12)))))

;;; Build an exercise.  Returns list of the exercises.
(define (make-exercise #:name name
                       #:intervals intervals
                       #:hands hands
                       #:style style)
  ;; The intervals as a list.
  (define list-intervals (decode-intervals intervals))

  (append*
    (for/list ([base (in-list bases)])
      (define midi-base (note-value base))
      (define gen (interval-gen midi-base list-intervals))

      (define notes-1 (make-scale #:notes (length list-intervals)
                                  #:octaves (style-octaves style)
                                  #:extra (style-extra style)))
      (define notes-2 (style-notes style notes-1))
      ;; Add the starting note back, _after_ adding the pattern.
      (define notes-2b (append notes-2 '(0)))
      (define notes-3 (map gen notes-2b))

      (for/list ([hand (in-list (handedness-names hands))]
                 [chordify (in-list (handedness-chord-gen hands))])
        (define chords (chordify notes-3))
        (exercise
          (format "~a-scale ~a ~a~a" hand base name (style-name style))
          (encode-exercise chords))))))

;;; Generate all of the exercises.
(define (all-exercises)
  (append
    (make-exercise #:name "major"
                   #:intervals "WWHWWWH"
                   #:hands hands-both-progressive
                   #:style style-up-down)
    (make-exercise #:name "minor (dorian)"
                   #:intervals "WHWWWHW"
                   #:hands hands-both
                   #:style style-up-down)
    (make-exercise #:name "dominant (mixolydian)"
                   #:intervals "WWHWWHW"
                   #:hands hands-both
                   #:style style-up-down)
    (make-exercise #:name "half diminished (locrian)"
                   #:intervals "HWWHWWW"
                   #:hands hands-both
                   #:style style-up-down)
    (make-exercise #:name "diminished (whole-half)"
                   #:intervals "WHWHWHWH"
                   #:hands hands-both-progressive
                   #:style style-up-down)
    (make-exercise #:name "sym-dom (half-whole)"
                   #:intervals "HWHWHWHW"
                   #:hands hands-both
                   #:style style-up-down)
    (make-exercise #:name "major"
                   #:intervals "WWHWWWH"
                   #:hands hands-both-progressive
                   #:style style-3-up)
    (make-exercise #:name "major"
                   #:intervals "WWHWWWH"
                   #:hands hands-both-progressive
                   #:style style-3-up-rev)
    (make-exercise #:name "whole-half"
                   #:intervals "WHWHWHWH"
                   #:hands hands-both
                   #:style style-3-up)
    (make-exercise #:name "whole-half"
                   #:intervals "WHWHWHWH"
                   #:hands hands-both
                   #:style style-3-up-rev)))
