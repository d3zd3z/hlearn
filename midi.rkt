#lang racket

(provide with-midi-in
         read-exercise)

;;; This uses the rtmidi package to retrieve a lesson.
;;; This library seems to group messages by packet, but I'm not sure
;;; this is guaranteed, so we will process them.
(require rtmidi)

;;; Open the midi interface, and return the handle.
(define (open-midi-in)
  (define in (make-rtmidi-in))
  (match (rtmidi-ports in)
    [(list) (error "There are no midi input devices")]
    [(list name)
     (printf "Using MIDI: ~a~%" name)]
    [(list-rest names)
     (printf "Multiple midi inputs: ~A, using first~%" names)])
  (rtmidi-open-port in 0)
  in)

(define close-midi-in rtmidi-close-port)

;;; The midi connection used in this thread.
(define midi-in (make-parameter #f))

(define (with-midi-in action)
  (define in (open-midi-in))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([midi-in in])
        (action)))
    (lambda ()
      (close-midi-in in))))

;;; These two constants define how the exercise is input.  This first,
;;; the chord-separation value distinguishes notes that we consider
;;; played together.  We consider 80ms to be approximately together,
;;; and this seems to work in the real world.
(define chord-time 0.080)

;;; How long without any activity should we wait for an event to be
;;; considered finished.
(define exercise-time 1.5)

;;; Using the current midi port, allow an exercise to be played.
;;; After draining any buffered input, wait for input.  Then, once the
;;; first message has been received, capture any midi events until we
;;; receive a timeout.  Only looks at note down and up events,
;;; although note up events are used to determine the end of the
;;; playing (in case something is held down).  We ignore everything
;;; else, especially since some devices will send "Active Sensing"
;;; regularly, and that should not affect our timing.
;;;
;;; Returns a list of pairs of timestamps and note-down values.
(define (read-exercise)
  (display "wait...")
  (flush-output)
  (let loop (
             ;; Last time we saw a message.  #f means none is seen, so no
             ;; timeout.
             [last-time (current-subseconds)]

             ;; Notes that we've seen down.  We won't finish the exercise until
             ;; everything that went down has been released.
             [down-notes (seteqv)]

             ;; All the notes recorded so far, in reverse order.
             [captured null]

             ;; An infinite stream for the cursor.
             [cursor (make-cursor)])

    ;; Check the timer, and continue waiting if we still have time
    ;; remaining.  event? is a boolean that indicates whether this is
    ;; a significant event, as far as the timeout goes.
    (define (timeout-continue down-notes new-captured event?)
      (define now (current-subseconds))
      (define new-timeout
        (if event? now last-time))
      (cond
        ;; The finish condition.
        [(and (set-empty? down-notes)
              (not (null? captured))
              (> now (+ last-time exercise-time)))
         (displayln "done.")
         (flush-output)
         (group-chords (reverse new-captured))]

        ;; Check for this being the first notes, and start the capture
        ;; message.
        [(and (null? captured)
              (not (null? new-captured)))
         (display "recording...")
         (flush-output)
         (loop new-timeout down-notes new-captured cursor)]

        ;; Otherwise, just continue waiting.
        [else
          (define cursor2
            (if event? (update-cursor cursor)
              cursor))
          (loop new-timeout down-notes new-captured cursor2)]))

    ;; Read from the midi device, with a timeout
    (match (sync/timeout 0.250 (midi-in))

      ;; Got the down note.  We ignore the channel, which could
      ;; falsely register notes if more than one device were sending.
      [(list _ (? note-down?) note _)
       ;; The timestamp from rtmidi seems completely meaningless.
       ;; Fortunately, it seems to return the notes in a timely
       ;; manner, so we'll just record our own timestamps.
       (timeout-continue (set-add down-notes note)
                         (cons (cons (current-subseconds) note) captured)
                         #t)]

      ;; Note up removes the note from those that are pressed.
      [(list _ (? note-up?) note _)
       (timeout-continue (set-remove down-notes note) captured #t)]

      ;; Anything else (including a timeout) just loops, checking the
      ;; timer.
      [_ (timeout-continue down-notes captured #f)])))

;;; Given a list of pairs, with the car being a second-based
;;; timestamp, and the cdr being a note value, group together all of
;;; the notes that are played within chord-time seconds of each other.
;;; I believe that 'group-by' compares against the first element.  The
;;; result discards the times, as it is no longer needed.  Within a
;;; chord, the notes are always in order.
(define (group-chords notes)
  (map (lambda (l) (sort (map cdr l) <))
       (group-by car notes
                 (lambda (a b)
                   (<= (abs (- b a)) chord-time)))))

;;; Get the current time in seconds, but get return a flonum that
;;; represents time less than a second.
(define (current-subseconds)
  (/ (current-milliseconds) 1000.0))

;;; Is this code a MIDI down note command.
(define (note-down? code)
  (= (bitwise-and code #xF0) #x90))

;;; Is this code a MIDI up note command.
(define (note-up? code)
  (= (bitwise-and code #xF0) #x80))

;;; Create a sequence that generates a spinning cursor, one char at a
;;; time.
(define (make-cursor) 0)

;;; Show the given cursor, and return a stream for the rest of the
;;; cursor.
(define (update-cursor cur)
  (display (string-ref "-\\|/" cur))
  (display #\backspace)
  (flush-output)
  (if (= cur 3) 0 (add1 cur)))
