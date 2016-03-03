#lang racket
;; ---------------------------------------------------------------------------------------------------
;; Usage
;;
;; (require "phrases.rkt")
;; (playphrase inthecagesolo .11))
;; ---------------------------------------------------------------------------------------------------

(require rsound)
(require "instruments.rkt")

(provide inthecagesolo)
(provide playphrase)

;; ---------------------------------------------------------------------------------------------------
;; Playing melodies, writing melodies as lists of symbols
;; ---------------------------------------------------------------------------------------------------

(define nonote -1)

(struct event (note numticks ampl) #:transparent)

(define nullevent 
  ;; 'Null event'
  (event -1 0 0))

(define (play-event-list instrument eventlist tick-duration)
  (let ([now (now)])
    (for ([e eventlist]
          [index (in-naturals)])
      (let* ([notenum (event-note e)]
             [notedur (* tick-duration (event-numticks e))]
             [ampl (event-ampl e)]
             [start-frame ((* index tick-duration) . seconds-from . now)])
        (when (not (eq? e nullevent))
          (myplay (instrument notenum notedur) start-frame ampl))))))

;; Sample usage:
;; (phrase-to-eventlist '(60 + + + + 60 60 + - 63 + - 65 + + + + 65 65 +))
(define (phrase-to-eventlist phrase)
  (reversephrase-to-eventlist (reverse phrase) 0 '()))

(define (reversephrase-to-eventlist phrase-start-in-reverse sustain eventlist-end)
  (if (empty? phrase-start-in-reverse)
      eventlist-end
      (let ([c (first phrase-start-in-reverse)])
        (cond
          [(eq? c '+)
           (reversephrase-to-eventlist 
            (rest phrase-start-in-reverse) 
            (+ 1 sustain) 
            (cons nullevent eventlist-end))]
          [(eq? c '/)
           (reversephrase-to-eventlist
            (rest phrase-start-in-reverse)
            sustain
            eventlist-end)]
          [(exact-positive-integer? c)
           (reversephrase-to-eventlist
            (rest phrase-start-in-reverse)
            0
            (cons (event c (+ 1 sustain) 1.0) eventlist-end))]
          [else
           (reversephrase-to-eventlist 
            (rest phrase-start-in-reverse) 
            0 
            (cons nullevent eventlist-end))]))))

;; Temporary definitions
;; Sample usage:
(define (playphrase phrase tick-duration) 
  (play-event-list mysynth (phrase-to-eventlist phrase) tick-duration))

;; Tests for playphrase
;; (Keyboard solo from "In the Cage")
;;
(define solo1 '(/ 63 65 67 68 70 72 74 75 / 77 75 77 75 70 75 77 75 /
                  77 75 69 75 77 75 77 75 / 68 75 77 75 77 75 70 75 /
                  66 75 77 75 70 75 77 75 / 66 75 77 75 70 75 77 75 /
                  65 75 77 69 75 77 72 75 / 77 69 75 77 65 77 65 77 /
                  66 75 77 75 70 75 77 75 / 66 75 77 75 70 75 77 75 /
                  65 75 77 69 75 77 70 75 / 77 68 75 77 67 77 65 -  /))
(define solo2 '(/ 63 65 67 68 70 72 74 75 / 74 72 70 +  72 70 68 +  /
                  70 68 67 +  68 67 65 67 / 68 70 72 74 75 77 79 80 /
                  82 80 79 77 79 80 82 80 / 82 80 79 77 79 80 82 83 /
                  82 78 77 78 75 71 70 71 / 82 78 77 78 75 71 70 71 /
                  82 78 77 78 75 71 70 71 / 82 78 77 78 75 71 70 71 /
                  70 66 65 66 63 59 58 59 / 58 +  +  +  +  +  +  +  /
                  +  +  +  +  +  +  +  +  /))
(define solo3 '(/ 70 +  66 +  65 +  66 +  / 63 +  59 +  58 +  59 +  /
                  56 +  52 +  51 +  52 +  / 49 +  45 +  44 +  45 +  /
                  46 +  +  +  47 +  +  +  / 54 +  +  +  53 +  +  +  /
                  51 +  +  +  +  +  49 +  / 44 +  +  +  46 +  +  +  /
                  +  +  +  +  47 +  +  +  / 54 +  +  +  53 +  +  +  /
                  48 +  +  +  49 +  +  +  / 54 +  +  +  +  +  53 +  /
                  51 +  +  +  +  +  +  +  / +  +  53 +  +  +  54 +  /
                  49 +  +  46 +  +  49 +  / 51 +  +  +  +  +  +  +  /))
(define solo4 '(/ 53 +  +  +  +  +  +  +  / +  +  +  +  +  +  +  +  /
                  49 +  48 +  49 +  51 +  / 48 +  +  44 +  +  48 +  /
                  46 +  +  +  +  +  +  +  / +  +  +  +  +  +  +  +  /
                  44 +  +  +  47 +  +  +  / 50 +  +  +  53 +  +  +  /
                  51 +  +  +  +  +  46 +  / 42 +  +  +  +  +  +  +  /
                  41 +  44 +  47 +  50 +  / 53 +  56 +  59 +  62 +  /
                  63 +  +  +  +  +  58 +  / 54 +  +  +  +  +  +  +  /
                  50 53 56 59 62 65 68 71 / 74 77 80 83 74 77 80 83 /
                  82 +  +  +  +  +  +  +  / +  +  +  +  +  +  +  +  /))

(define inthecagesolo (append solo1 solo2 solo3 solo4))