#lang racket/base

(require rsound
         rsound/piano-tones
         (prefix-in rds: rsound/drum-samples) ;; rds: Rsound Drum Samples
         rsound/single-cycle
         racket/class)

(provide $synth
         $bloop
         $major-chords
         $chords
         $wobble
         $drums
         ;; These are currently only needed by phrases.rkt
         ;; TODO migrate phrases.rkt to use proper instruments and not need these?
         now
         seconds-from
         myplay
         mysynth)

;; ---------------------------------------------------------------------------------------------------
;; Useful constants
;; ---------------------------------------------------------------------------------------------------

(define mystream
  (make-pstream))

(define (now)
  (pstream-current-frame mystream))

;; (5 . seconds-from . (now))
(define (seconds-from t start)
  (+ start (floor (* t (default-sample-rate)))))

(define (myplay sound frame ampl) 
  (pstream-queue mystream
                 (rs-scale ampl sound)
                 frame))

;; ---------------------------------------------------------------------------------------------------
;; Basic sounds
;; ---------------------------------------------------------------------------------------------------

(define (makesynth n)
  (λ (note dur)
    (synth-note "main" n note (round (* dur (default-sample-rate))))))

(define mysynth (makesynth 15))

;; ---------------------------------------------------------------------------------------------------
;; Arpeggios and chords
;; ---------------------------------------------------------------------------------------------------

(define arps #hash(("M"   . (0 4 7 12 7 4 0))
                   ("m"   . (0 3 7 12 7 3 0))
                   ("dim" . (0 3 6 12 6 3 0))
                   ("7"   . (0 4 7 10 7 4 0))
                   ("M7"  . (0 4 7 11 7 4 0))
                   ("m7"  . (0 3 7 10 7 3 0))))

(define chords #hash(("M"   . (0 4 7 12))
                     ("m"   . (0 3 7 12))
                     ("dim" . (0 3 6 12))
                     ("7"   . (0 4 7 10))
                     ("M7"  . (0 4 7 11))
                     ("m7"  . (0 3 7 10))))

#|(define (randomnotes n low high)
  (build-list n (λ x (+ low (random (- high low))))))

(define (pick list)
  (list-ref list (random (length list))))

(define (pickn list n)
  (build-list n (λ x (pick list))))

|#


(define (myplay/arp instrument root-notenum deltas notedur)
  (let ([now (now)])
    (for ([delta deltas]
          [index (in-naturals)])
      (let ([notenum (+ root-notenum delta)]
            [start-frame ((* index notedur) . seconds-from . now)])
        (myplay (instrument notenum notedur) start-frame 0.5)))))

(define (myplay/chord instrument root-notenum deltas notedur)
  (let ([now (now)])
    (for ([delta deltas])
      (let ([notenum (+ root-notenum delta)])
        (myplay (instrument notenum notedur) now (/ 0.5 (length deltas)))))))

(define (myplay/namedchord instrument root-notenum chordname notedur)
  (let* ([now (now)]
         [deltas (hash-ref chords chordname)]
         [ampl (/ 0.5 (length deltas))])
    (for ([delta deltas])
      (let ([notenum (+ root-notenum delta)])
        (myplay (instrument notenum notedur) now ampl)))))


(define (myplay/namedchord2 instrument root-notenum chordname notedur)
  (let* ([now (now)]
         [deltas (hash-ref chords chordname)]
         [ampl (/ 0.5 (length deltas))]
         [sounds (map (λ (delta) (instrument (+ root-notenum delta) notedur)) deltas)])
    (map (λ (sound) (myplay sound now ampl)) sounds)))

(define (notenum-to-freq notenum)
  (* 440 (expt 2 (/ (- notenum 57) 12))))

;; ---------------------------------------------------------------------------------------------------
;; Instrument definitions
;; ---------------------------------------------------------------------------------------------------

(define $synth
  (class object% (super-new)
    (define/public (play notenum)
      (myplay (mysynth notenum .1) (now) 0.5))))

(define $bloop
  (class object% (super-new)
    (define/public (play notenum)
      (myplay/arp mysynth notenum '(0 7 12 7 0) .075))))

(define $major-chords
  (class object% (super-new)
    (define/public (play notenum)
      (myplay/chord mysynth notenum '(0 4 7 12) .5))))

(define $chords
  (class object% (super-new)
    (define/public (play notenum chordname)
      (myplay/namedchord mysynth notenum chordname 2.0))))

;; TODO Calling (stop) in here makes the other instruments stop working
;; I need to either fix the other existing instruments, or never call
;; (stop) and instead just set the amplitude of this instrument to 0?

;; TODO Buffered playback of this instrument - perhaps
;; when playing, it can actually create a sound and queue it for playback,
;; and do this continuously via a callback. When told to stop, instead of
;; calling (stop), it just interrupts the callback. This might also solve
;; the problem above (though it would still be nice to figure out why (stop)
;; stops all playback to the stream)


(define $wobble
  (class object% (super-new)
    (define -is-playing? #f)
    (define -base-frequency 400)
    (define -my-network
      (network ()
               [lfo <= sine-wave 10]
               [sin <= sine-wave (+ -base-frequency (* 10 lfo))]
               [out = (* 0.1 sin)]))
    (define/public (play notenum)
      (begin
        (set! -base-frequency (notenum-to-freq notenum))
        (when (not -is-playing?)
          (begin
            (set! -is-playing? #t)
            (signal-play -my-network)))))
    (define/public (stop-playback)
      (when -is-playing?
        (begin
          (stop)
          (set! -is-playing? #f))))))

(define $drums
  (class object% (super-new)
    (define/public (kick)
      (myplay rds:kick (now) .5))
    (define/public (bassdrum)
      (myplay rds:bassdrum (now) .5))
    (define/public (bassdrum-synth)
      (myplay rds:bassdrum-synth (now) .5))))
