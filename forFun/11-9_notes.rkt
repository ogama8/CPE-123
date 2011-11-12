#lang racket

(require (planet clements/rsound))


(define msr mono-signal->rsound)

(define (osc freq amp)
  (local [(define (sig f)
            (* amp (sin (* 2 pi freq f 1/44100))))]
    sig))

(define (sig+ sig1 sig2)
  (local [(define (sig f)
          (+ (sig1 f) (sig2 f)))]
    sig))

(define (sig* sig1 sig2)
  (local [(define (sig f)
          (* (sig1 f) (sig2 f)))]
    sig))

(define (k amp)
  (local [(define (sig f) amp)]
    sig))

(define lfo-1 (osc 32 1))

(define (osc/f freq-sig amp)
  (local [(define (sig f)
            (* amp (sin (* 2 pi (freq-sig f) f 1/44100))))]
    sig))

(play (msr (* 10 44100)
           (sig* lfo-1
                 (sig+ (sig+ (osc 130 .2)
                             (osc 260 .4))
                       (osc 520 .6)))))

#;(play (msr (* 5 44100)
                 (osc/f (sig+ (k 130)
                              (osc 8 50)) .2)))



(define (wobble control subsig)
  (local [(define (sig f)
            (subsig (+ f (control f))))]
    sig))

#;(play (msr (* 10 44100)
                 (wobble (sig* (osc .1 1) 
                               (osc 10 1000)) 
                         (osc 100 .2))))