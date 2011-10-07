#lang racket

;; Exercise 20
(define (string-join str1 str2) 
  (string-append str1 "_" str2))

(string-join "good" "day")


;; Exercise 21
(define (string-insert str x) 
  (string-append (substring str 0 x) 
                 "_" 
                 (substring str x 
                            (string-length str))))

(string-insert "goodday" 4)


;; 4 Sound
(require (planet "main.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (* 44.1 x))

;; Stutter
(define (eight snd) (clip snd 0 (round (* (rsound-frames snd) 1/8))))
(define (stutter rs) (rs-append* (for/list ([i (in-range 8)]) (eight rs))))

;;(play (stutter (rs-read/clip "/Users/Eli/Documents/CP Fall 2011/CPE-123/Assignment1/hg.wav" 0 441000)))


;; Chord
;; (make-tone pitch(hz) vol(0-1) frames)
(define (chord x y z) (overlay (make-tone x 1/4 (ms 1000)) 
                               (overlay (make-tone y 1/4 (ms 1000)) 
                                        (make-tone z 1/4 (ms 1000)))))

;;(play (chord 440 (* 440 3/2) (* 440 5/4)))

;; Classify
(define (classify x y) (cond [(or (= (/ x y) (/ 3 2)) (= (/ x y) (/ 2 3))) "fifth"]
                             [(or (or (= (/ x y) (/ 5 4)) (= (/ x y) (/ 4 5))) (or (= (/ x y) (/ 6 5)) (= (/ x y) (/ 5 6)))) "third"]
                             [else "unknown"]))

(classify 440 (* 440 3/2))


;; Noisy
(define (noisy n) (/ (- (random 100) 50) 200))

;;(play (mono-signal->rsound 44100 noisy))



;; Squarewave
(define(squarewave n) (cond [(= (/ (truncate (/ n 100)) 2) 
                                (truncate (/ (truncate (/ n 100)) 2))) 10000]
                            [else 0]))

;;(play (mono-signal->rsound 44100 squarewave))