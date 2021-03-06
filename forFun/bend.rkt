;;Eli Backer - Fall 2011

;; Initial Requires/Defines
#lang racket
(require (planet clements/rsound))
(require (planet "draw.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (floor (* x 441 1/10)))

;; bend: number number number -> rsound
 ; Bend takes 2 pitches and number of samples and produces a sound with a starting pitch
 ; and ending pitch of those defined, for the number of samples requested
(define (bend pitch1 pitch2 length)
  (local [(define (tone n)
            (cond [(< pitch1 pitch2) (sin (* 2 n pi 1/44100 (+ pitch1 (* (- pitch2 pitch1) (/ n length)))))]
                  [(> pitch1 pitch2) (sin (* 2 n pi 1/44100 (+ pitch1 (* 1/2 (- pitch2 pitch1) (/ n length)))))]))]
    (mono-signal->rsound length tone)))
                                       
(play (bend 400 0 (ms 10000)))