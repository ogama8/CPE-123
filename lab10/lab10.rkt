;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Eli Backer - Lab 10 - Clements - CPE 123 - Fall 2011

;; Initial Requires/Defines
(require (planet "main.rkt" ("clements" "rsound.plt" 2 5)))
(require (planet "draw.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (floor (* x 441 1/10)))

(define my-sound (rs-read/clip "/Users/Eli/Documents/CP Fall 2011/CPE-123/lab5/my.wav" 0 (ms 10000)))

;; reverse-sound rsound -> rsound
 ; Given rsound -> rsound in reverse
(define (reverse-sound rs)
  (local [(define (function n)
          (rs-ith/left rs (- (- (rsound-frames rs) n) 1)))]
    (mono-signal->rsound (rsound-frames rs) function)))

(check-within (rs-ith/left (reverse-sound my-sound) 
                           (+ -1 (rsound-frames (reverse-sound my-sound))))
              (rs-ith/left my-sound 1)
              1e-4)

#;(play (reverse-sound my-sound))



;; reverse-odd-seconds rsound -> rsound
 ; Given rsound of length greater than 1 second -> rsound with odd second segments reversed
(define (reverse-odd-seconds rs)
  (local [(define (function n)
            (cond [(odd? (ceiling (/ n 44100))) (rs-ith/left rs (- (* 44100 (ceiling (/ n 44100))) (- n 1)))]
                  [(even? (ceiling (/ n 44100))) (rs-ith/left rs n)]))]
    (mono-signal->rsound (rsound-frames rs) function)))

(check-within (rs-ith/left (reverse-odd-seconds my-sound) 44100)
              (rs-ith/left my-sound 1)
              1e-2)
(check-within (rs-ith/left (reverse-odd-seconds my-sound) 44101)
              (rs-ith/left my-sound 44101)
              1e-2)

(play (reverse-odd-seconds my-sound))