;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require (planet "main.rkt" ("clements" "rsound.plt" 2 5)))
(require (planet "draw.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (* 44.1 x))

;; t
;;(check-within (t 60) -.961 .001)
(define (t n) (cos (* 2 pi 400 n 1/44100)))

(check-within (t 60) -.961 .001)

;;(play (mono-signal->rsound (ms 1000) t))
;;(rsound-draw (mono-signal->rsound (ms 1000) t))

(define (sum-t a b c)
  (local ((define (t f) (* (+ (cos (* 2 pi a f 1/44100)) (cos (* 2 pi b f 1/44100)) (cos (* 2 pi c f 1/44100))) 1/3)))
    t))

;;(play (mono-signal->rsound (ms 1000) (sum-t 200 400 800)))
;;(rsound-draw (mono-signal->rsound (ms 1000) (sum-t 200 400 800)))



;; Music Fragment
(define soundLocation "/Users/Eli/Dropbox/sunvox/songs/End 2011 PCM.wav")
(define (cut loc) (rs-read/clip loc 4000 4200))

;;(play (times 100 (cut soundLocation)))
;;(rsound-draw (times 100 (cut soundLocation)))


;; My T
(define (my-t freq)
  (local ((define (tone n) 
            (- (sin (* n pi 1/44100 freq)) 1)))
    tone))

(play (mono-signal->rsound (ms 1000) (my-t 440)))
;;(rsound-draw (mono-signal->rsound 20000 (my-t 440)))