;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Eli Backer - Lab 9 - Clements - CPE 123 - Fall 2011

;; Initial Requires/Defines
(require (planet "main.rkt" ("clements" "rsound.plt" 2 5)))
(require (planet "draw.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (* x 44.1))



;; ring: number number -> rsound
 ; Given frequnecy and time produces sine with decay of 4*time
(define (ring f t)
  (local ((define (tone n) (* (expt 1/2 (/ n t)) (sin (* 2 n pi 1/44100 f)))))
    (mono-signal->rsound (* 4 t) tone)))

;(rsound-draw (ring 440 (ms 500)))
;(play (ring 220 (ms 500)))



;; beat: number number -> rsound
 ; Given frequency and time produces 2 sines with a beat of frequency t (time)
 ; Lasts for 4t
(define (beat f t)
  (local ((define (tone n) (* 1/2 (+ (sin (* 2 n pi 1/44100 f))
                                     (sin (* 2 n pi 1/44100 (- f (/ 44100 t))))))))
    (mono-signal->rsound (* 4 t) tone)))

;(rsound-draw (beat 220 (ms 1000)))
;(play (beat 220 (ms 1000)))



;; note [number or booleean] -> sound (void)
 ; Given MIDI number -> 1/4 sec tone at pitch
 ; Given false -> 1/4 sec silence
(define (note nn)
  (cond ([false? nn] (play (silence (ms 250))))
        (else (local ((define (tone n) (sin (* 2 n pi 1/44100 (* 440 (expt 2 (/ (- nn 69) 12)))))))
                (play (mono-signal->rsound (ms 250) tone))))))

;(note 60)