;;Eli Backer - Lab 9 part 2 - Clements - CPE 123 - Fall 2011

;; Initial Requires/Defines
#lang racket
(require (planet clements/rsound))
(define (ms x) (* x 441 1/10))

;; note [number or booleean] -> rsound
 ; Given MIDI number -> 1/4 sec tone at pitch
 ; Given false -> 1/4 sec silence
(define (note nn)
  (cond ([false? nn] (play (silence (ms 250))))
        (else (local ((define (tone n) (sin (* 2 n pi 1/44100 (* 440 (expt 2 (/ (- nn 69) 12)))))))
                (mono-signal->rsound (ms 250) tone)))))

;; notes->song: list -> rsound
 ; Given list of MIDI notes or false, produces that sequence of notes
(define (notes->song noteList)
  (rs-append* (for/list ([rs (in-list noteList)])
                (note rs))))

;; Note List Edelweiss
(define edelweiss (list 61 61 64 71 71 71 70 70 64 62 62 false))


;;Expect Edelweiss
(play (notes->song edelweiss))