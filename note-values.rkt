#lang racket

(provide string->midi-value
         midi-value->string)

;;; Converting midi values to note names and back.

;;; Midi zero is C(-2), although the negative ranges aren't commonly used.
;;; These are the "canonical" note names, e.g., those made with Unicode values for ♯ and ♭.  There
;;; are two variants depending on whether we prefer sharps or flats.
(define note-names-sharp '#("C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯" "A" "A♯" "B"))
(define note-names-flat '#("C" "D♭" "D" "E♭" "E" "F" "G♭" "G" "A♭" "A" "B♭" "B"))

;;; Letter names.
(define letter-values (hash "C" 0 "D" 2 "E" 4 "F" 5 "G" 7 "A" 9 "B" 11))
(define accidental-values (hash "" 0 #f 0
                                "#" 1 "##" 2 "♯" 1 "♯♯" 2 "𝄪"
                                2 "b" -1 "bb" -2 "♭" -1 "𝄫" -2))

(define (string->midi-value text)
  (match text
    [(pregexp #px"^([abcdefgABCDEFG])([𝄪𝄫]|[#b♯♭]{1,2})?(\\d)$"
             (list _ name accidental octave))
     (+ (hash-ref letter-values (string-upcase name))
        (hash-ref accidental-values accidental)
        (* (string->number octave) 12)
        12)]
    [else (error "Unknown note name" text)]))

(define (midi-value->string num [bias 'sharps])
  (define names (case bias
                  [(sharps) note-names-sharp]
                  [(flats) note-names-flat]
                  [else (error "Invalid bias, must be 'sharps or 'flags" bias)]))
  (define-values (octave index)
    (let loop ([value num]
               [octave -1])
      (if (>= value 12)
        (loop (- value 12) (add1 octave))
        (values octave value))))
  (string-append (vector-ref names index) (number->string octave)))
