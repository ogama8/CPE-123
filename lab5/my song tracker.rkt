#lang racket

(require (planet clements/rsound)
         racket/runtime-path
         rackunit)

;; Define locations
(define home-dir "/Volumes/ELI'S KEY/CPE 123/")
(define (file x) (string-append home-dir x ".wav"))
(define (read x) (rs-read (file x)))

;; Define extra sounds
(define my1 (read "my1"))
(define my2 (read "my2"))
(define my3 (read "my3"))
(define my4 (read "my4"))
(define my5 (read "my5"))

;; locate the pattern file
(define-runtime-path src-file "percussion2.txt")

;; define some constants
(define tempo 60)
(define secondsperbeat (/ 60 tempo))
(define framesperbeat (* (default-sample-rate) 
                         secondsperbeat))
(define colsperbeat 4)
(define framespercol (/ framesperbeat colsperbeat))

;; here's the list of sounds to be used.
(define raw-sounds (list my1
                         my2
                         my3
                         my4
                         my5
                         clap-2
                         c-hi-hat-1))

;; make the sounds quieter, so they won't cause clipping
(define sounds (map (lambda (s) (scale 0.2 s)) raw-sounds))

;; convert a file to a list of list of booleans
(define (file->bool-rows file)
  (map line->bools (file->lines file)))

;; convert a string to a list of booleans,
;; where every non-space character produces true
(define (line->bools str)
  (for/list ([ch (in-list (string->list str))])
    (not (equal? ch #\space))))

;; convert a list of booleans to a list of 
;; sound/offset lists, for use with assemble
(define (bools->overlay-list bools sound)
  (for/list ([b (in-list bools)]
             [t (in-naturals)]
             #:when b)
    (list sound (* t framespercol))))

;; read the file and play the resulting sound
(define (go)
  (define bool-rows (file->bool-rows src-file))
  ;; take only enough rows to match the sounds given
  (define num-taken (min (length bool-rows)
                         (length sounds)))
  (define used-bool-rows (take bool-rows num-taken))
  (define used-sounds (take sounds num-taken))
  (define sound/offsets (map
                         bools->overlay-list
                         used-bool-rows
                         used-sounds))
  (define s (assemble (apply append sound/offsets)))
  (play s))


;; read the file and record the resulting sound
(define (rec)
  (define bool-rows (file->bool-rows src-file))
  ;; take only enough rows to match the sounds given
  (define num-taken (min (length bool-rows)
                         (length sounds)))
  (define used-bool-rows (take bool-rows num-taken))
  (define used-sounds (take sounds num-taken))
  (define sound/offsets (map
                         bools->overlay-list
                         used-bool-rows
                         used-sounds))
  (define s (assemble (apply append sound/offsets)))
  (rs-write s (string-append home-dir "myf.wav")))

;; a test case for line->bools
(check-equal? (line->bools "x x ") 
              (list #t #f #t #f))

;; a test case for bools->overlay-list
(check-equal? (bools->overlay-list (list false true false true)
                                   kick)
              (list (list kick (* 1 framespercol))
                    (list kick (* 3 framespercol))))

