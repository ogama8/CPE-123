;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Assignment 1
;; Eli Backer, Max Parelius, Cameron Javier

(require (planet "main.rkt" ("clements" "rsound.plt" 2 5)))
(define home-dir "/Volumes/ELI'S KEY/CPE 123/")

(define (ms x) (* x 44.1))
(define (file x) (string-append home-dir x ".wav"))
(define (read x) (rs-read (file x)))

;;(rs-write
;;(play 
;; (rs-read (file "hg4"))
;; (rs-read/clip (file "hg") (ms 26800) (ms 28800))
;;"/Volumes/ELI'S KEY/CPE 123/hg4.wav"
;;)

(play (rs-append* (list (read "hg1") (silence (ms 500)) 
                        (read "hg2") (silence (ms 500)) 
                        (read "hg3") (silence (ms 500)) 
                        (read "hg4") (silence (ms 500)) 
                        (read "hg5"))))