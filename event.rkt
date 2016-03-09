#lang racket

;; Structures and functions to manage events and event lists

(provide eventlist-insert
         eventlist-remove
         align)

(struct event (time instrument command))


;; Each event is assumed to be of the form
;;  (list t ...)
;; where t represents the time of the event (in milliseconds)
;; It is assumed that eventlist contains events ordered by their t,
;; and we want to insert event into eventlist in the correct place,
;; to preserve this order
(define (eventlist-insert el e)
  (define (helper head e tail)
    (cond
      [(empty? tail) (append head (list e))]
      [(< (car e) (first (first tail))) (append head (list e) tail)]
      [else (helper (append head (list (first tail))) e (rest tail))]))
  (helper '() e el))

;; Each event is assumed to be of the form
;;  (list t id ...)
;; where t represends the time of the event (in milliseconds)
;; and id represents an identifier for the event
(define (eventlist-remove el id)
  (filter (λ (e) (not (eq? (second e) id))) el))


;; align divides the range from 0..length up into num-ticks parts
;; bounded by (num-ticks + 1) equally-spaced "ticks", and determines which
;; of these ticks is closest to t. The first and last tick are considered
;; equivalent, so if it turns out to be closest to the last tick
;; (at time length), we say it's at time 0
;;
;; Examples:
;;  (align 11 100 10) -> 10
;;  (align 11 100 5)  -> 20
;;  (align 40 100 3)  -> 33
;;
(define (align t length num-ticks)
  (let* ([tick-times (map (λ (k) (quotient (* k length) num-ticks)) (range (+ 1 num-ticks)))]
         [distance-to-t (λ (tick-time) (abs (- tick-time t)))]
         [nearest-tick (argmin distance-to-t tick-times)])
    (if (eq? nearest-tick length)
        0
        nearest-tick)))

