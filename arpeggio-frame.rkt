#lang racket/gui
;; ---------------------------------------------------------------------------------------------------
;; Usage
;;
;; (require "arpeggio-frame.rkt")
;; (send arpeggio-frame show #t)
;; ---------------------------------------------------------------------------------------------------

(require "instruments.rkt")
(provide arpeggio-frame)

(define arpeggio-frame
  (new frame%
       [label "Arpeggios"]))

(define (makebutton panel notenum notename arpname)
  (new button%
       [parent panel]
       [label (string-append notename arpname)]
       [callback (lambda (button event)
                   (myplay/arp mysynth notenum (hash-ref arps arpname) .1))]))

; Tests for makebutton
#;(makebutton panel 60 "C" "M")
#;(makebutton panel 60 "C" "m")
#;(for ([(arpname notelist) arps])
  (makebutton panel 60 "C" arpname))


(define (makebuttonrow parent notenum notename)
  (let ([panel (new horizontal-panel% [parent parent])])
    (for ([arpname '("M" "m" "dim" "7" "M7" "m7")])
      (makebutton panel notenum notename arpname))
    panel))



(makebuttonrow arpeggio-frame 60 "C")
(makebuttonrow arpeggio-frame 61 "C#")
(makebuttonrow arpeggio-frame 62 "D")
(makebuttonrow arpeggio-frame 63 "D#")
(makebuttonrow arpeggio-frame 64 "E")
(makebuttonrow arpeggio-frame 65 "F")
(makebuttonrow arpeggio-frame 66 "F#")
(makebuttonrow arpeggio-frame 67 "G")
(makebuttonrow arpeggio-frame 68 "G#")
(makebuttonrow arpeggio-frame 69 "A")
(makebuttonrow arpeggio-frame 70 "A#")
(makebuttonrow arpeggio-frame 71 "B")

