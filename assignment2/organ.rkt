#lang racket

(require (planet clements/rsound/main) 
         (planet clements/rsound/draw)
         (planet clements/rsound/filter)
         racket/runtime-path)

;; FILE CONVENTION FOR .TRACKER FILES
 ; TITLE
 ; BPM
 ; NOTE FOR BEAT (ie. 2 = 1/2 note, 4 = 1/4 note, etc)
 ;
 ; NOTE (UPPERCASE = NATURAL, lowercase = sharp (possibles: c d f g a))
 ; OR . OR NOTHING (. HOLDS NOTE, NOTHING STOPS TONE)
 ; ...
 ; * (INDICATES END OF SEQUENCE)

(define-runtime-path trkLoc "organ.tracker")
(define-runtime-path trkLoc2 "organ2.tracker")
(define-runtime-path trkLoc3 "organ3.tracker")

(define (ms n) (* 44.1 n))
(define msr mono-signal->rsound)

;; Basic Sine Wave
(define (osc freq amp)
  (local [(define (sig f)
            (* amp (sin (* 2 pi freq f 1/44100))))]
    sig))

;; Signal Operators
(define (sig+ sig1 sig2)
  (local [(define (sig f)
          (+ (sig1 f) (sig2 f)))]
    sig))

(define (sig* sig1 sig2)
  (local [(define (sig f)
          (* (sig1 f) (sig2 f)))]
    sig))

;; Constant as Function
(define (k amp)
  (local [(define (sig f) amp)]
    sig))

;; nName->Hz: string -> number
 ; Given a string of a note (see above for convention) produces a frequency
(define (nName->hz nnOct) 
  (local [(define nn (substring nnOct 0 1))
          (define oct (string->number (substring nnOct 1 2)))]
    (cond ([string=? nn "S"] 0)
          (else (* 440 (expt 2 (/ (- (+ (* (+ 1 oct) 12) 
                            (cond ([string=? nn "C"] 0)
                                  ([string=? nn "c"]  1)
                                  ([string=? nn "D"] 2)
                                  ([string=? nn "d"]  3)
                                  ([string=? nn "E"] 4)
                                  ([string=? nn "F"] 5)
                                  ([string=? nn "f"]  6)
                                  ([string=? nn "G"] 7)
                                  ([string=? nn "g"]  8)
                                  ([string=? nn "A"] 9)
                                  ([string=? nn "a"]  10)
                                  ([string=? nn "B"] 11))) 69) 12)))))))


;; tfAtt: number -> number
 ; Given a number, produces a number in the function of a thousend frame attack, then constant value
(define (tfAtt f) 
  (cond ([< f 999] (* (+ 1 f) 1/1000))
        (else 1)))

;; nlFil: string -> bool
 ; Given an empty string, produces false, else true
(define (nlFil str) (not (string=? str "")))

;; track->rs: file function -> rsound
 ; Given .tracker file and a signal generator function -> rsound
(define (track->rs floc)
  (local [(define trackStr (filter nlFil (file->lines floc)))
          (define bpm (string->number (list-ref trackStr 1)))
          (define beat (string->number (list-ref trackStr 2)))]
    (rs-append* 
     (for/list ([i (in-range (- (length trackStr)
                                4))])
                (local [(define len (round (* 44100 
                                                 (/ 60 bpm (* beat 2)) 
                                                 (string->number (substring (list-ref trackStr (+ 4 i)) 
                                                                            3
                                                                            (string-length (list-ref trackStr (+ 4 i))))))))
                        (define freq (nName->hz (list-ref trackStr (+ 4 i))))
                        (define (sig f)
                          (cond ([< f (- len 1000)] 
                                 (* (+ .9 (* .1 (sin (* 2 pi freq (* f 1/30) 1/44100)))) 
                                    (+ -.5 (* 1 1/6 (- 1 (sin (* 2 pi freq f 1/44100))))
                                       (* .85 1/6 (- 1 (sin (* 2 pi 2 freq f 1/44100))))
                                       (* .5 1/6 (- 1 (sin (* 2 pi 4 freq f 1/44100))))
                                       (* .4 1/6 (- 1 (sin (* 2 pi 6 freq f 1/44100))))
                                       (* .35 1/6 (- 1 (sin (* 2 pi 8 freq f 1/44100))))
                                       (* .05 1/6 (- 1 (sin (* 2 pi 12 freq f 1/44100)))))))
                                (else (* (/ (- len f) 1000)
                                         (+ .9 (* .1 (sin (* 2 pi freq (* f 1/30) 1/44100))))
                                         (+ -.5 (* 1 1/6 (- 1 (sin (* 2 pi freq f 1/44100))))
                                            (* .85 1/6 (- 1 (sin (* 2 pi 2 freq f 1/44100))))
                                            (* .5 1/6 (- 1 (sin (* 2 pi 4 freq f 1/44100))))
                                            (* .4 1/6 (- 1 (sin (* 2 pi 6 freq f 1/44100))))
                                            (* .35 1/6 (- 1 (sin (* 2 pi 8 freq f 1/44100))))
                                            (* .05 1/6 (- 1 (sin (* 2 pi 12 freq f 1/44100)))))))))]
                  (msr len (lpf/dynamic tfAtt sig)))))))

(play (overlay (times 28 (track->rs trkLoc3))
               (overlay (times 28 (track->rs trkLoc2))
                        (track->rs trkLoc))))

#;(rs-write (overlay (times 28 (track->rs trkLoc3))
               (overlay (times 28 (track->rs trkLoc2))
                        (track->rs trkLoc))) "/Users/Eli/Documents/CP Fall 2011/CPE-123/assignment2/organ.wav")