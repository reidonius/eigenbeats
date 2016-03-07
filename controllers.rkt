#lang racket/gui

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

#| recording-controller-mixin

Usage:
  (define my-recording-controller%
    (recording-controller-mixin my-original-controller))

Summary:
  A mixin which extends a controller% by allowing recording and playback
  of key events. It maintains an internal state (to capture whether we
  are recording, playing back, etc) and listens for a few key events which
  can manipulate that state, forwarding the remaining key events on to
  its parameter class.

  TODO document interface and state transitions.
  TODO factor out states/state transitions, and move -loop-start-time
       to be a member of the 'timing state, since it's not needed for
       anything else (i.e it doesn't really belong as a member of this class)
|#
(define (recording-controller-mixin %)
  (class % (super-new)
    (inherit set-canvas-background
             refresh
             key->event
             get-instrument)
    ;; The -state field takes one of the following values:
    ;;   'free      Notes are played normally, no recording
    ;;   'timing    Notes are played normally,
    ;;   'dub       Notes are "recorded" and played back again at the
    ;;              same time through the loop
    ;;   'playback  Notes are played normally (not recorded),
    ;;              and recorded notes are played back
    (define -state 'free)
    (define -loop-start-time #f)
    (define loop-player #f)
    (define/override (on-char ke)
      (let ([k (send ke get-key-code)])
        (match (list k -state)
          ;; Some key events below trigger state transitions in this controller
          [(list #\space 'free)
           (begin (set! -state 'timing)
                  (set! -loop-start-time (current-milliseconds))
                  (set-canvas-background verylightred)
                  (refresh))]
          [(list #\space 'timing)
           (begin (set! -state 'dub)
                  (let ([loop-length (- (current-milliseconds) -loop-start-time)])
                    (set! loop-player (new loop-player%
                                           [initial-frame-start-time -loop-start-time]
                                           [frame-length loop-length])))
                  (set-canvas-background lightred)
                  (refresh))]
          [(list #\space 'dub)
           (begin (set! -state 'playback)
                  (set-canvas-background lightblue)
                  (refresh))]
          [(list #\space 'playback)
           (begin (set! -state 'dub)
                  (set-canvas-background lightred)
                  (refresh))]
          [(list #\z 'dub)
           (send loop-player remove-last-recorded-event)]
          [(list #\x 'dub)
           (send loop-player toggle-quantization)]
          [(list #\backspace _)
           (begin (set! -state 'free)
                  (when loop-player
                    (send loop-player stop-loop-thread)
                    (send loop-player clear-events))
                  (set-canvas-background lightgreen)
                  (refresh))]
          ;; Other key events are forwarded to the superclass, but we "record" them by
          ;;  * Asking the superclass if this is a meaningful key event (i.e. does it trigger an instrument event)
          ;;  * If so, saving the event in our list
          [(list _ _)
           (begin (when (and (eq? -state 'dub)
                             (list? (key->event k)))
                    (send loop-player record-event (current-milliseconds) (get-instrument) (key->event k)))
                  (super on-char ke))])))))

(define recording-instrument-controller%
  (recording-controller-mixin instrument-controller%))

;; quantize divides the range from 0..length up into num-ticks parts
;; bounded by (num-ticks + 1) equally-spaced "ticks", and determines which
;; of these ticks is closest to t. The first and last tick are considered
;; equivalent, so if it turns out to be closest to the last tick
;; (at time length), we say it's at time 0
;;
;; Examples:
;;  (quantize 11 100 10) -> 10
;;  (quantize 11 100 5)  -> 20
;;  (quantize 40 100 3)  -> 33
;;
(define (quantize t length num-ticks)
  (let* ([tick-times (map (λ (k) (quotient (* k length) num-ticks)) (range (+ 1 num-ticks)))]
         [distance-to-t (λ (tick-time) (abs (- tick-time t)))]
         [nearest-tick (argmin distance-to-t tick-times)])
    (if (eq? nearest-tick length)
        0
        nearest-tick)))

;; Each event is assumed to be of the form
;;  (list t ...)
;; where t represents the time of the event (in milliseconds)
;; It is assumed that eventlist contains events ordered by their t,
;; and we want to insert event into eventlist in the correct place,
;; to preserve this order
(define (insert-to-eventlist event eventlist)
  (define (helper eventlist-head event eventlist-tail)
    (cond
      [(empty? eventlist-tail) (append eventlist-head (list event))]
      [(< (car event) (first (first eventlist-tail))) (append eventlist-head (list event) eventlist-tail)]
      [else (helper (append eventlist-head (list (first eventlist-tail))) event (rest eventlist-tail))]))
  (helper '() event eventlist))

;; Each event is assumed to be of the form
;;  (list t id ...)
;; where t represends the time of the event (in milliseconds)
;; and id represents an identifier for the event
(define (remove-id-from-eventlist id eventlist)
  (filter (λ (event) (not (eq? (second event) id))) eventlist))
  

;; A loop-player% manages looped recording and playback of instrument events
(define loop-player%
  (class object% (super-new)
    (init initial-frame-start-time)
    (init frame-length)
    (define -current-frame-start-time initial-frame-start-time)
    (define -frame-length frame-length)
    (define -quantize-new-events? #f)
    (define -num-ticks 16)
    (define -recorded-events '())
    ;; Calling remainder here probably isn't necessary, because
    ;; -current-frame-start-time should be kept up to date by the -loop-thread below.
    ;; However, it's possible that this function gets called more than one full
    ;; frame-length after -current-frame-start-time, if -loop-thread has not had
    ;; the chance to advance to the next frame yet
    (define (-time-within-frame time)      
      (remainder (- time -current-frame-start-time) -frame-length))
    (define -loop-thread
      (thread (lambda ()
                (printf "Loop-player: Loop thread started. Frame length: ~a\n" -frame-length)
                ;; The main loop - traverses the list of recorded events, playing each one
                ;; at the appropriate time, and jumping back to the start of the list when
                ;; we get to a new frame
                (let play-pending ([pending -recorded-events])
                  (let* ([time-within-current-frame (- (current-milliseconds) -current-frame-start-time)]
                         [advance-frame? (> time-within-current-frame -frame-length)]
                         [play-event-now? (λ (event) (< (car event) time-within-current-frame))])
                    (if (empty? pending)
                        (if advance-frame?
                            (begin
                              (set! -current-frame-start-time (+ -current-frame-start-time -frame-length))
                              (sleep .01)
                              (play-pending -recorded-events))
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
    (define/public (record-event time instrument command)
      (let* ([event-time (if -quantize-new-events?
                             (quantize (-time-within-frame time) -frame-length -num-ticks)
                             (-time-within-frame time))]
             [event-id (+ 1 (length -recorded-events))]
             [new-event (list event-time event-id instrument command)]
             [new-recorded-events (insert-to-eventlist new-event -recorded-events)])
        (set! -recorded-events new-recorded-events)
        (printf "Loop-player: Added event ~a at time ~a\n" event-id event-time)))
    (define/public (clear-events)
      (set! -recorded-events '()))
    (define/public (remove-last-recorded-event)
      (when (not (empty? -recorded-events))
        (let ([id-to-remove (length -recorded-events)])
          (set! -recorded-events (remove-id-from-eventlist id-to-remove -recorded-events))
          (printf "Loop-player: Removed event ~a\n" id-to-remove))))
    (define/public (toggle-quantization)
      (set! -quantize-new-events? (not -quantize-new-events?))
      (printf "Loop-player: Quantization ~a\n" (if -quantize-new-events? "ON" "OFF")))
    (define/public (normalized-frame-position)
      (/ (-time-within-frame (current-milliseconds)) (exact->inexact -frame-length)))
    (define/public (stop-loop-thread)
      (kill-thread -loop-thread)
      (printf "Loop-player: Loop thread killed\n"))))