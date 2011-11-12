;;Eli Backer - Lab 10 part 2 - Clements - CPE 123 - Fall 2011

;; Initial Requires/Defines
#lang racket
(require rackunit)
(require (planet "main.rkt" ("clements" "rsound.plt" 2 5)))
(require (planet "draw.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (floor (* x 441 1/10)))



;; rescale: rsound number -> rsound
 ; Given rsound and number, produces rsound stretched by factor of number
(define (rescale sound n)
  (local [(define f (/ 1 n))]
    (rs-append* (for/list ([i (in-range (floor (/ (rsound-frames sound) f)))])
                  (clip sound (inexact->exact (floor (* i f))) (inexact->exact (floor (+ 1 (* i f)))))))))

(define kickRS (rs-read "/Users/Eli/Documents/CP Fall 2011/CPE-123/lab10/VEH2 70s and 80s Kicks/VEH2 70s and 80s Kicks - 01.wav"))

(check-= (rsound-frames (rescale kickRS .5))
         (floor (/ (rsound-frames kickRS) 2))
         1e-4)

;(play (rescale kickRS 1))


(define vehKicksList (for/list ([n (in-range 33)])
                      (string-append "/Users/Eli/Documents/CP Fall 2011/CPE-123/lab10/VEH2 70s and 80s Kicks/VEH2 70s and 80s Kicks - "
                                     (cond ([> 10 (+ n 1)] (string-append "0" (number->string (+ n 1))))
                                           ([<= 10 (+ n 1)] (number->string (+ n 1))))
                                     ".wav")))

;; orgNoise List -> rsound
 ; Takes a list of wav file locations, stretches them rancomly to a value bet. .5 and 2, 
 ; plays them randomly for a total of 20 sounds over 30 seconds producing an rsound.
(define (orgNoise listRS)
  (assemble (for/list ([i (in-range 20)])
              (local ((define sound (rs-read (list-ref listRS (random (length listRS)))))
                      (define rand (+ 1/2 (* 1/20000 (random 30000)))))
                (list (rs-append* (for/list ([i (in-range (floor (/ (rsound-frames sound) rand)))])
                                    (clip sound (floor (* i rand)) (floor (+ 1 (* i rand))))))
                      (random (- (ms 30000) (rsound-frames sound))))))))

(play (orgNoise vehKicksList))
#;(rsound-draw (orgNoise vehKicksList))