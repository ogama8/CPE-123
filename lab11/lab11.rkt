#lang racket
;; Eli Backer - Lab 11 - Clements - CPE 123 - Fall 2011

;; Initial Requires/Defines
(require (planet "main.rkt" ("clements" "rsound.plt" 2 5)))
(require (planet "draw.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (round (* x 441 1/10)))
(define (samplesOfHz x) (round (/ 44100 x)))

(define tones 
  (assemble (list 
             (list (make-tone 400 .05 (ms 5000)) 0)
             (list (make-tone 450 .05 (ms 4000)) (ms 1000))
             (list (make-tone 500 .05 (ms 3000)) (ms 2000))
             (list (make-tone 550 .05 (ms 2000)) (ms 3000)))))
;;(play tones)

;; delay-by-n: rsound number -> rsound
 ; Given rsound and frames produces rsound with copy added and delayed by number (frames)
(define (delay-by-n sound delay)
  (assemble (list
             (list (scale .5 sound) 0)
             (list (scale .5 sound) delay))))


;; Filtering 2 and 4
#;(play (delay-by-n tones (samplesOfHz 100)))


;; Filtering 2
(play (delay-by-n tones (samplesOfHz 900)))