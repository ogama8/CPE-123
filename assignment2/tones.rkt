#lang racket

(require (planet clements/rsound/main) 
         (planet clements/rsound/draw)
         racket/runtime-path)

(define-runtime-path midFile "lead.mid")

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


;; Hopefully an Organ-like Noise
(define (org freq amp/sig)
  (local [(define (sig f)
            (* (amp/sig f) (+ -.5 (* 1 1/6 (- 1 (sin (* 2 pi freq f 1/44100))))
                                (* .85 1/6 (- 1 (sin (* 2 pi 2 freq f 1/44100))))
                                (* .5 1/6 (- 1 (sin (* 2 pi 4 freq f 1/44100))))
                                (* .4 1/6 (- 1 (sin (* 2 pi 6 freq f 1/44100))))
                                (* .35 1/6 (- 1 (sin (* 2 pi 8 freq f 1/44100))))
                                (* .05 1/6 (- 1 (sin (* 2 pi 12 freq f 1/44100)))))))]
    sig))

#;(play (msr (ms 5000)(org 220 (sig+ (k .9) (osc 10 .1)))))

(define midStr (file->bytes midFile))

midStr