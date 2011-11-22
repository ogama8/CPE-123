#lang racket
;; Lab 14 - CPE 123 - Eli Backer - Clements - Fall 2011

(define (ms x) (* x 44.1))

; a chord-pair is (make-chord-pair tone1 tone2 dur)
(define-struct chord-pair (tone1 tone2 dur))

(chord-pair 440 220 (ms 1000))
(chord-pair 100 1000 10)

; a chord is (make-chord list dur)
(define-struct chord (list dur))

(chord (list 69 10 100 3) (ms 10000))
(chord (list 55) 1340)

; a key-signature is (make-key-signature f c g d a e b)
(define-struct key-signature (f c g d a e b))

(key-signature "sharp" "sharp" "sharp" "sharp" "sharp" "sharp" "natural")
(key-signature "natural" "natural" "natural" "natural" "natural" "natural" "flat")

; a chord-sequence is (make-chord-sequence seq vol)
(define-struct chord-sequence (seq vol))

(chord-sequence (list (chord (list 69 10 100 3) (ms 10000)) 
                      (chord (list 69 10 100 3) (ms 10000)) 
                      (chord (list 69 10 100 3) (ms 10000))) 
                1)
(chord-sequence (list (chord (list 1) 1)) 0)
