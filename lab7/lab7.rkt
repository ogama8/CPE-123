;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;#lang racket
(require (planet "main.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (* 44.1 x))

;; Up Maj. 3rd
;;(check-expect (up-maj-3 440) 554)
(define (up-maj-3 freq) (inexact->exact (round (* (expt 2 4/12) freq))))

(check-expect (up-maj-3 440) 554)


;; Up Maj. 3rd MIDI
;;(check-expect (up-maj-3/nn 69) 73)
(define (up-maj-3/nn nn) (+ 4 nn))

(check-expect (up-maj-3/nn 69) 73)


;; Num. to Octave
;;(check-expect (note-num->octave 60) 4)

(define (note-num->octave note-num) (- (/ (- note-num (modulo note-num 12)) 12) 1))

(check-expect (note-num->octave 60) 4)


;; Num. to Name
;;(check-expect (note-num->name 60) "c")
(define (m12 x) (modulo x 12))
(define (note-num->name note-num) (cond ([= (m12 note-num) 0] "c")
                                        ([= (m12 note-num) 1] "c♯")
                                        ([= (m12 note-num) 2] "d")
                                        ([= (m12 note-num) 3] "d♯")
                                        ([= (m12 note-num) 4] "e")
                                        ([= (m12 note-num) 5] "f")
                                        ([= (m12 note-num) 6] "f♯")
                                        ([= (m12 note-num) 7] "g")
                                        ([= (m12 note-num) 8] "g♯")
                                        ([= (m12 note-num) 9] "a")
                                        ([= (m12 note-num) 10] "a♯")
                                        ([= (m12 note-num) 11] "b")))
(check-expect (note-num->name 61) "c♯")


;; Name to Num.
;; (check-expect (name->note-num "d♯" 2) 39)
(define (name->note-num nn oct) (+ (* (+ 1 oct) 12) 
                                   (cond ([string=? nn "c"] 0)
                                         ([or (string=? nn "c♯") (string=? nn "d♭")]  1)
                                         ([string=? nn "d"] 2)
                                         ([or (string=? nn "d♯") (string=? nn "e♭")]  3)
                                         ([string=? nn "e"] 4)
                                         ([string=? nn "f"] 5)
                                         ([or (string=? nn "f♯") (string=? nn "g♭")]  6)
                                         ([string=? nn "g"] 7)
                                         ([or (string=? nn "g♯") (string=? nn "a♭")]  8)
                                         ([string=? nn "a"] 9)
                                         ([or (string=? nn "a♯") (string=? nn "b♭")]  10)
                                         ([string=? nn "b"] 11))))
(check-expect (name->note-num "e♭" 2) 39)



;;  Name to Rsound
;; 440 * 2^((n-69)/12)
;;(check-expect (name->rsound "a" 4) 440)
(define (name->rsound nn oct) 
  (local ((define (tone f) (sin (* f 2 pi 1/44100 (* 440 (expt 2 (/ (- (name->note-num nn oct) 69) 12)))))))
  (mono-signal->rsound 22050 tone)))

(play (name->rsound "a" 4))
