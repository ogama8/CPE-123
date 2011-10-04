#lang racket

(require (planet clements/rsound)
         racket/runtime-path
         rackunit)

;; Custom definitions
(define sound-directory "/Volumes/ELI'S KEY/CPE 123/")
(define (wav-rs name) (rs-read (string-append sound-directory name ".wav")))

;; Define extra sounds
(define hg1 (wav-rs "hg1"))
(define hg2 (wav-rs "hg2"))
(define hg3 (wav-rs "hg3"))
(define hg4 (wav-rs "hg4"))
(define hg5 (wav-rs "hg5"))

;; Define locations
(define home-dir "/Volumes/ELI'S KEY/CPE 123/")
(define (file x) (string-append home-dir x ".wav"))

;; locate the pattern file
(define-runtime-path src-file "percussion.txt")

;; define some constants
(define tempo 60)
(define secondsperbeat (/ 60 tempo))
(define framesperbeat (* (default-sample-rate) 
                         secondsperbeat))
(define colsperbeat 4)
(define framespercol (/ framesperbeat colsperbeat))

;; here's the list of sounds to be used.
(define raw-sounds (list hg1
                         hg2
                         hg3
                         hg4
                         hg5
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
  (rs-write s (string-append home-dir "hs-all.wav")))

;; a test case for line->bools
(check-equal? (line->bools "x x ") 
              (list #t #f #t #f))

;; a test case for bools->overlay-list
(check-equal? (bools->overlay-list (list false true false true)
                                   kick)
              (list (list kick (* 1 framespercol))
                    (list kick (* 3 framespercol))))

