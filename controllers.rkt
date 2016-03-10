#lang racket/gui

(require "event.rkt")

(provide controller%
         instrument-controller%
         recording-instrument-controller%)

;; Colors
(define white         (make-object color% 255 255 255))
(define lightblue     (make-object color% 224 240 255))
(define lightgreen    (make-object color% 224 255 240))
(define verylightred  (make-object color% 255 240 240))
(define lightred      (make-object color% 255 224 224))

;; A controller% is just a canvas that accepts tab focus and changes color
;; when gaining or losing focus
(define controller%
  (class canvas% (super-new)
    (inherit accept-tab-focus
             set-canvas-background
             refresh)
    (accept-tab-focus #t)
    (define/override (on-focus on?)
      (if on?
          (begin (set-canvas-background lightgreen)
                 (refresh))
          (begin (set-canvas-background white)
                 (refresh))))))

#|
An instrument-controller% extends controller% by imbuing it with
  * An instrument
  * A mapping from character codes to events associated with the instrument
|#
(define instrument-controller%
  (class controller% (super-new)
    (init instrument)
    (init key-to-event-map)
    (define -instrument instrument)
    (define -key-to-event-map key-to-event-map)
    (define/public (key->event k)
      (hash-ref -key-to-event-map k #f))
    (define/public (get-instrument)
      -instrument)
    (define/override (on-char ke)
      (let* ([k (send ke get-key-code)]
             [event-with-args (key->event k)])
        (when (list? event-with-args)
          (apply dynamic-send (cons -instrument event-with-args)))))))

#| recording-instrument-controller%

Usage:
  (define my-recording-instrument-controller
    (new recording-instrument-controller%))

Summary:
  A class which extends a controller% by allowing recording and playback
  of key events. It maintains an internal state (to capture whether we
  are recording, playing back, etc) and listens for a few key events which
  can manipulate that state, forwarding the remaining key events on to
  its parameter class.

  TODO document interface and state transitions.
  TODO factor out states/state transitions, and move -loop-start-time
       to be a member of the 'timing state, since it's not needed for
       anything else (i.e it doesn't really belong as a member of this class)
|#
(define recording-instrument-controller%
  (class instrument-controller%
    (init name)
    (inherit set-canvas-background
             key->event
             get-instrument
             get-width
             get-height)
    ;; The -state field takes one of the following values:
    ;;   'free      Notes are played normally, no recording
    ;;   'dub       Notes are "recorded" and played back again at the
    ;;              same time through the loop
    (define -name name)
    (define -state 'free)
    (define -align-new-events? #t)
    (define -recorded-events '())
    (define -num-ticks 16)
    (define -loop-player the-loop-player)
    (define (-record-event time instrument command align?)
      (let* ([event-time (if align?
                             (align (send -loop-player time-within-frame time)
                                    (send -loop-player get-frame-length)
                                    -num-ticks)
                             (send -loop-player time-within-frame time))]
             [event-id (+ 1 (length -recorded-events))]
             [new-event (list event-time event-id instrument command)]
             [new-recorded-events (eventlist-insert -recorded-events new-event)])
        (set! -recorded-events new-recorded-events)
        (send -loop-player update-eventlist -recorded-events)
        (printf "Recording-controller: Added event ~a at time ~a\n" event-id event-time)))
    (define (-remove-last-recorded-event)
      (when (not (empty? -recorded-events))
        (let ([id-to-remove (length -recorded-events)])
          (set! -recorded-events (eventlist-remove -recorded-events id-to-remove))
          (send -loop-player update-eventlist -recorded-events)
          (printf "Recording-controller: Removed event ~a\n" id-to-remove))))
    (define/override (on-char ke)
      (let ([k (send ke get-key-code)])
        (match (list k -state)
          ;; Some key events below trigger state transitions in this controller
          [(list #\\ 'free)
           (begin (set! -state 'dub)
                  (set-canvas-background lightred)
                  (printf "Recording-controller: Recording is ON\n"))]
          [(list #\\ 'dub)
           (begin (set! -state 'free)
                  (set-canvas-background lightgreen)
                  (printf "Recording-controller: Recording is OFF\n"))]
          [(list #\space _)
           (if (send -loop-player is-playing?)
               (send -loop-player pause)
               (send -loop-player unpause))]
          ;; ..Others modify properties of the controller
          [(list #\x _)
           (begin (set! -align-new-events? (not -align-new-events?))
                  (printf "Recording-controller: Alignment ~a\n" (if -align-new-events? "ON" "OFF")))]
          [(list #\< _)
           (begin (if (> -num-ticks 1)
                      (set! -num-ticks (sub1 -num-ticks))
                      (printf "Recording-controller: # ticks for alignment: ~a\n" -num-ticks)))]
          [(list #\> _)
           (begin (set! -num-ticks (add1 -num-ticks))
                  (printf "Recording-controller: # ticks for alignment: ~a\n" -num-ticks))]
          [(list #\backspace 'dub)
           (begin (-remove-last-recorded-event)
                  (send -loop-player update-eventlist -recorded-events))]
          ;; ..The rest are forwarded to the superclass, but if in the 'dub state,
          ;;   we "record" them by
          ;;    * Asking the superclass if this is a meaningful key event (i.e. does it trigger an instrument event)
          ;;    * If so, saving the event in our list of recorded-events
          [(list _ _)
           (begin (super on-char ke)
                  (when (and (eq? -state 'dub)
                             (list? (key->event k)))
                    (-record-event (current-milliseconds) (get-instrument) (key->event k) -align-new-events?)
                    (send -loop-player update-eventlist -recorded-events)))])))
    (define/override (on-focus on?)
      (if on?
          (if (eq? -state 'dub)
              (set-canvas-background lightred)
              (set-canvas-background lightgreen))
          (set-canvas-background white)))
    ;; TODO make this more efficient - instead of redrawing everything every time,
    ;; maybe only re-draw the sweeping line every time, and pre-render everything else
    ;; on a differt buffer, only re-drawing it when it needs to?
    (define/private (my-paint-callback canvas dc)
      ;; Draw the name
      (send dc draw-text -name 0 0)
      ;; Draw the "ticks"
      (let ([tick-x-positions (map (λ (k) (/ (* k (get-width)) -num-ticks)) (range (add1 -num-ticks)))]
            [y-top (* (get-height) 1/4)]
            [y-bot (* (get-height) 3/4)])
        (send dc set-pen "black" 1 'solid)
        (for ([x tick-x-positions])
          (send dc draw-line x y-top x y-bot)))
      ;; Draw the recorded events
      (let* ([get-event-x-position (λ (event) (/ (* (get-width) (first event)) (send -loop-player get-frame-length)))]
             [event-x-positions (map get-event-x-position -recorded-events)])
        (send dc set-brush "blue" 'solid)
        (send dc set-pen "white" 1 'transparent)
        (for ([x event-x-positions])
          (send dc draw-ellipse (- x 4) (- (/ (get-height) 2) 4) 8 8)))
      ;; Draw the sweeping progress line
      (let ([line-x-position (* (get-width) (send -loop-player normalized-frame-position))])
        (send dc set-pen "red" 2 'solid)
        (send dc draw-line line-x-position 0 line-x-position (get-height))))
    (super-new
     (paint-callback (λ (canvas dc) (my-paint-callback canvas dc))))))


;; A loop-player% manages looped playback of a list of instrument events
(define loop-player%
  (class object% (super-new)
    (init initial-frame-start-time)
    (init frame-length)
    (define -current-frame-start-time initial-frame-start-time)
    (define -frame-length frame-length)
    (define -looped-events '())
    (define -pause-time-within-frame #f)
    (define -paused? #f)
    ;; Calling remainder here probably isn't necessary, because
    ;; -current-frame-start-time should be kept up to date by the -playback-thread below.
    ;; However, it's possible that this function gets called more than one full
    ;; frame-length after -current-frame-start-time, if -playback-thread has not had
    ;; the chance to advance to the next frame yet
    (define/public (time-within-frame time)
      (if -paused?
          -pause-time-within-frame
          (remainder (- time -current-frame-start-time) -frame-length)))
    (define -playback-thread
      (thread (lambda ()
                (printf "Loop-player: Loop thread started. Frame length: ~a\n" -frame-length)
                ;; The main loop - traverses the list of looped events, playing each one
                ;; at the appropriate time, and jumping back to the start of the list when
                ;; we get to a new frame
                (let play-pending ([pending -looped-events])
                  (let* ([time-within-current-frame (- (current-milliseconds) -current-frame-start-time)]
                         [advance-frame? (> time-within-current-frame -frame-length)]
                         [play-event-now? (λ (event) (< (car event) time-within-current-frame))])
                    (if (empty? pending)
                        (if advance-frame?
                            (begin
                              (set! -current-frame-start-time (+ -current-frame-start-time -frame-length))
                              (sleep .01)
                              (play-pending -looped-events))
                            (begin
                              (sleep .01)
                              (play-pending pending)))
                        (let ([next-event (first pending)])
                          (if (play-event-now? next-event)
                              (begin
                                (apply dynamic-send (cons (third next-event) (fourth next-event)))
                                (sleep .01)
                                (play-pending (cdr pending)))
                              (begin
                                (sleep .01)
                                (play-pending pending))))))))))
    (define/public (update-eventlist new-eventlist)
      (set! -looped-events new-eventlist))
    (define/public (normalized-frame-position)
      (/ (time-within-frame (current-milliseconds)) (exact->inexact -frame-length)))
    (define/public (get-frame-length)
      -frame-length)
    (define/public (is-playing?)
      (thread-running? -playback-thread))
    ;; TODO It would be nice to use fewer variables to record this paused/unpaused state.
    ;; It should be thread safe w.r.t. the drawing thread, due to the order in which
    ;; things are modified, but that's a slippery slope...
    (define/public (pause)
      (when (not -paused?)
        (thread-suspend -playback-thread)
        (set! -pause-time-within-frame (time-within-frame (current-milliseconds)))
        (set! -current-frame-start-time #f)
        (set! -paused? #t)
        (printf "Loop-player: Paused\n")))
    (define/public (unpause)
      (when -paused?
        (set! -current-frame-start-time (- (current-milliseconds) -pause-time-within-frame))
        (set! -pause-time-within-frame #f)
        (thread-resume -playback-thread)
        (set! -paused? #f)
        (printf "Loop-player: Unpaused\n")))))

(define the-loop-player
  (new loop-player%
       [initial-frame-start-time (current-milliseconds)]
       [frame-length 2400]))