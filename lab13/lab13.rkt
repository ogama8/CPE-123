#lang racket
;; Lab 13 - Eli Backer - CPE 123 - Clements - Fall 2011

(require (planet clements/rsound/main) 
         (planet clements/rsound/draw)
         (planet clements/rsound/filter)
         racket/runtime-path
         rackunit)

(define msr mono-signal->rsound)
(define (one n) 1)

(define sndList (list (msr 100 one)
                      (msr 100 one)
                      (msr 100 one)
                      (msr 100 one)
                      (msr 100 one)))

;; sum-of-durations: list[rsounds] -> number
 ; given a list of rsounds -> total length of all sounds in list
(define (sum-of-durations list)
  (for/fold ([total 0])
    ([n (in-range (length list))])
    (+ total (rsound-frames (list-ref list n)))))

(check-= (sum-of-durations sndList) 
         500
         1e-4)


;; sum: list[numbers] -> number
 ; Given a list of numbers returns the sum
(define (sum list)
  (for/fold ([total 0])
    ([n (in-range (length list))])
    (+ total (list-ref list n))))

(check-= (sum (list 1 2 3 4 5))
         15
         1e-4)

;; durations: list[rsounds] -> list[numbers]
 ; Given a list of rsounds, produces a list of numerical durations
(define (durations list)
  (for/list ([n (in-range (length list))])
    (rsound-frames (list-ref list n))))

(check-equal? (durations sndList)
         (list 100 100 100 100 100))


;; sum-of-durations2: list[rsounds] -> number
 ; Given a list of rsounds -> total length of all sounds in list
(define (sum-of-durations2 list)
  (sum (durations list)))

(check-= (sum-of-durations2 sndList) 
         500
         1e-4)


;; maybe-tone: number -> [rsound or bool]
 ; Given a midi nn betweeen 60 and 70 produces an rsound of corresponding pitch
 ; otherwise, returns false
(define (maybe-tone nn)
  (cond ([and (<= nn 70) 
               (>= nn 60)]
         (make-tone (* 440 (expt 2 (* 1/12 (+ -69 nn)))) .2 (round 44100/4)))
        (else #f)))

(check-equal? (maybe-tone 69)
              (make-tone 440 .2 (round 44100/4)))
(check-equal? (maybe-tone 10)
              #f)
(check-equal? (maybe-tone 100)
              #f)

;; maybe-tones: list -> rsound
 ; Given a list of midi nn's produces
(define (maybe-tones list)
  (rs-append* (for/list ([n (in-range (length list))])
                (local [(define read (maybe-tone (list-ref list n)))]
                  (cond ([equal? read #f] (silence 0))
                        (else read))))))

(check rsound-equal? (maybe-tones (list 61 65 99 10 70))
              (rs-append* (list (make-tone (* 440 (expt 2 (* 1/12 (+ -69 61)))) .2 (round 44100/4))
                                (make-tone (* 440 (expt 2 (* 1/12 (+ -69 65)))) .2 (round 44100/4))
                                (make-tone (* 440 (expt 2 (* 1/12 (+ -69 70)))) .2 (round 44100/4)))))