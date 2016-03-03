#lang racket/gui
;; ---------------------------------------------------------------------------------------------------
;; Usage
;;
;; (require "keyboard-frame.rkt")
;; (send keyboard-frame show #t)
;; ---------------------------------------------------------------------------------------------------

(require "instruments.rkt"
         "controllers.rkt")
(provide keyboard-frame)

;; Window that lets you add/remove subwindows with keystrokes

;; Parent frame
(define keyboard-frame
  (new frame%
       [label "Keyboard"]))

;; Top-level panel
(define top-panel
  (new horizontal-panel%
       [parent keyboard-frame]))

;; Given an instrument, and a map from character literals to messages,
;; responds to each keyboard event corresponding to a character literal
;; by sending the instrument the message found in the map
(define (add-recording-instrument-controller instrument key-to-event-map name)
  (new recording-instrument-controller%
       [instrument instrument]
       [key-to-event-map key-to-event-map]
       [parent instrument-controller-panel]
       [min-width 400]
       [min-height 50]
       [stretchable-width #t]
       [stretchable-height #t]
       [paint-callback
        (lambda (canvas dc)
          (send dc draw-text name 0 0))]))

(define instrument-controller-panel
  (new vertical-panel%
       [parent top-panel]
       [spacing 2]))

(define note-keymap
  #hash([#\a . (play 55)]
        [#\w . (play 56)]
        [#\s . (play 57)]
        [#\e . (play 58)]
        [#\d . (play 59)]
        [#\f . (play 60)]
        [#\t . (play 61)]
        [#\g . (play 62)]
        [#\y . (play 63)]
        [#\h . (play 64)]
        [#\j . (play 65)]
        [#\i . (play 66)]
        [#\k . (play 67)]
        [#\o . (play 68)]
        [#\l . (play 69)]
        [#\p . (play 70)]
        [#\; . (play 71)]
        [#\' . (play 72)]))


(define chord-keymap
  #hash([#\a . (play 55 "m")]
        [#\A . (play 55 "M")]
        [#\w . (play 56 "m")]
        [#\W . (play 56 "M")]
        [#\s . (play 57 "m")]
        [#\S . (play 57 "M")]
        [#\e . (play 58 "m")]
        [#\E . (play 58 "M")]
        [#\d . (play 59 "m")]
        [#\D . (play 59 "M")]
        [#\f . (play 60 "m")]
        [#\F . (play 60 "M")]
        [#\t . (play 61 "m")]
        [#\T . (play 61 "M")]
        [#\g . (play 62 "m")]
        [#\G . (play 62 "M")]
        [#\y . (play 63 "m")]
        [#\Y . (play 63 "M")]
        [#\h . (play 64 "m")]
        [#\H . (play 64 "M")]
        [#\j . (play 65 "m")]
        [#\J . (play 65 "M")]
        [#\i . (play 66 "m")]
        [#\I . (play 66 "M")]
        [#\k . (play 67 "m")]
        [#\K . (play 67 "M")]
        [#\o . (play 68 "m")]
        [#\O . (play 68 "M")]
        [#\l . (play 69 "m")]
        [#\L . (play 69 "M")]
        [#\p . (play 70 "m")]
        [#\P . (play 70 "M")]
        [#\; . (play 71 "m")]
        [#\: . (play 71 "M")]
        [#\' . (play 72 "m")]
        [#\" . (play 72 "M")]))

(define wobble-keymap
  #hash([#\a     . (play 55)]
        [#\w     . (play 56)]
        [#\s     . (play 57)]
        [#\e     . (play 58)]
        [#\d     . (play 59)]
        [#\f     . (play 60)]
        [#\t     . (play 61)]
        [#\g     . (play 62)]
        [#\y     . (play 63)]
        [#\h     . (play 64)]
        [#\j     . (play 65)]
        [#\i     . (play 66)]
        [#\k     . (play 67)]
        [#\o     . (play 68)]
        [#\l     . (play 69)]
        [#\p     . (play 70)]
        [#\;     . (play 71)]
        [#\'     . (play 72)]
        [#\.     . (stop-playback)]))

(define drum-keymap
  #hash([#\a  . (kick)]
        [#\s  . (bassdrum)]
        [#\d  . (bassdrum-synth)]))

(add-recording-instrument-controller (new $synth)
                                     note-keymap
                                     "$synth")

(add-recording-instrument-controller (new $bloop)
                                     note-keymap
                                     "$bloop")

(add-recording-instrument-controller (new $major-chords)
                                     note-keymap
                                     "$major-chords")

(add-recording-instrument-controller (new $chords)
                                     chord-keymap
                                     "$chords")

(add-recording-instrument-controller (new $wobble)
                                     wobble-keymap
                                     "$wobble")

(add-recording-instrument-controller (new $drums)
                                     drum-keymap
                                     "$drums")
