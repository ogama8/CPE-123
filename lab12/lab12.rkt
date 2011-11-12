#lang racket

;; Initial Requires/Defines
(require (planet "main.rkt" ("clements" "rsound.plt" 2 5)))
(require (planet "draw.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (round (* x 441 1/10)))
(define (samplesOfHz x) (/ 44100 x))

;;Finding Zeroes
; -0.176957-0.804697i
; -0.176957+0.804697i ***
; 0.492249-0.474239i 
; 0.492249+0.474239i
; -0.630583
; The second root is closest to the circle with a magnitude of 0.836010779849.

;; Random Root Filter Design
(define i (sqrt -1))
(define tones 
  (assemble (list 
             (list (make-tone 3500 .2 (ms 5000)) 0)
             (list (make-tone 4500 .2 (ms 4000)) (ms 1000))
             (list (make-tone 7503 .2 (ms 3000)) (ms 2000))
             (list (make-tone 7803 .2 (ms 2000)) (ms 3000)))))

;; my-fir: rsound number number number number number number -> rsound
 ; Given rsound and 4 scalars and delays (frames)
(define (my-fir sound d1 s1 d2 s2 d3 s3 d4 s4)
  (assemble (list
             (list (scale (/ 1 5) sound) 0)
             (list (scale s1 sound) d1)
             (list (scale s2 sound) d2)
             (list (scale s3 sound) d3)
             (list (scale s4 sound) d4))))

(play (my-fir tones 3 (/ -2.48903 5) 
              2 (/ 3.42044 5)
              1 (/ -2.48903 5)
              4 (/ 1.00000 5)))

