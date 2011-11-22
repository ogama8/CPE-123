#lang racket
;; Lab 15 - CPE 123 - Eli Backer - Clements - Fall 2011

(require (planet clements/rsound/main) 
         (planet clements/rsound/draw)
         (planet clements/rsound/filter)
         racket/runtime-path
         rackunit)
(define (ms x) (* x 44.1))

(define akGuitarList 
  (for/list ([n (in-range 38)])
    (string-append "/Users/Eli/Documents/CP Fall 2011/CPE-123/lab15/AKWF_aguitar/AKWF_aguitar_"
                   (cond ([> 10 (+ n 1)] (string-append "000" (number->string (+ n 1))))
                         ([<= 10 (+ n 1)] (string-append "00" (number->string (+ n 1)))))
                   ".wav")))

;; a note is either
 ; (make-note nn vol dur)
 ; (make-silence dur)
(define-struct midiNote (nn dur))
(define-struct noSound (dur))

(midiNote 69 (ms 1000))
(noSound 56)
(midiNote 10 4410)


;; rePitch: rsound number -> rsound
 ; given an rsound and number, scales rsound by number, producing a rsound
(define (rePitch rs scale)
  (local [(define (f x) 
            (rs-ith/left rs (inexact->exact (floor (/ x scale)))))]
    (mono-signal->rsound (floor (* (rsound-frames rs) scale)) f)))

;; notes->sound: (list note note ...) -> rsound
 ; Given a list of notes, produces corrispoding chord
(define (notes->sound noteList)
  (for/fold ([totRS (make-tone 1 0 1)])
    ([n (in-range (length noteList))])
    (overlay (scale .5 totRS)
             (cond ([noSound? (list-ref noteList n)] 
                    (make-tone 1 0 0))
                   ([midiNote? (list-ref noteList n)] 
                    (times (inexact->exact (round (/ (midiNote-dur (list-ref noteList n)) 
                                                     (rsound-frames (rePitch (rs-read (list-ref akGuitarList 1))
                                                                             (/ 36.70809598967594 
                                                                                (* 440 (expt 2 (* 1/12 (+ -69 (midiNote-nn (list-ref noteList n)))))))))))) 
                           (scale .5 (rePitch (rs-read (list-ref akGuitarList 1))
                                       (/ 36.70809598967594 
                                          (* 440 (expt 2 (* 1/12 (+ -69 (midiNote-nn (list-ref noteList n)))))))))))))))

#;(play (notes->sound (list (noSound (ms 1000)) (midiNote (- 69 12) (ms 1000)) (midiNote (- 69 12 12) (ms 1000)))))


;; sounds-nice-with-60?: note -> bool
 ; accepts a note and returns true if it is not 1, 2, or 6 half-steps away from midi note number 60. Silence always sounds nice
(define (sounds-nice-with-60? note)
  (cond ([midiNote? note] (not (or (= 54 (midiNote-nn note))
                              (= 58 (midiNote-nn note))
                              (= 59 (midiNote-nn note))
                              (= 61 (midiNote-nn note))
                              (= 62 (midiNote-nn note))
                              (= 66 (midiNote-nn note)))))
        ([noSound? note] #t)))

(check-equal? (sounds-nice-with-60? (midiNote 69 1))
              #t)
(check-equal? (sounds-nice-with-60? (noSound 1))
              #t)
(check-equal? (sounds-nice-with-60? (midiNote 61 1))
              #f)


;; sounds-nice/2: note note -> bool
 ; Same as above, but with 2 notes instead of a note and 60
(define (sounds-nice/2 note note2)
  (cond ([and (midiNote? note2) (midiNote? note)] 
         (not (or (= 1 (abs (- (midiNote-nn note) (midiNote-nn note2))))
                  (= 2 (abs (- (midiNote-nn note) (midiNote-nn note2))))
                  (= 6 (abs (- (midiNote-nn note) (midiNote-nn note2)))))))
        (else #t)))

(check-equal? (sounds-nice/2 (midiNote 69 1) (midiNote 66 1))
              #t)
(check-equal? (sounds-nice/2 (noSound 1) (midiNote 69 1))
              #t)
(check-equal? (sounds-nice/2 (midiNote 61 1) (midiNote 60 1))
              #f)


;; sounds-nice/3: note note note -> bool
 ; same as 2 but with 3 notes
(define (sounds-nice/3 note note2 note3)
  (and (sounds-nice/2 note note2)
       (sounds-nice/2 note2 note3)
       (sounds-nice/2 note3 note)))

(check-equal? (sounds-nice/3 (midiNote 69 1) (midiNote 66 1) (midiNote 62 1))
              #t)
(check-equal? (sounds-nice/3 (midiNote 69 1) (midiNote 66 1) (noSound 1))
              #t)
(check-equal? (sounds-nice/3 (midiNote 69 1) (midiNote 67 1) (midiNote 63 1))
              #f)


;; random-note: void -> note
 ; produces a note.  30% chance of silence, equal chance it is between 60 and 72 inclusive
(define (random-note) 
    (cond ([<= (random 10) 3] (noSound (ms (* 100/100000 (+ 50000 (random 150000))))))
          (else (midiNote (+ 60 (random 12)) (ms (* 100/100000 (+ 50000 (random 150000))))))))

"THIS IS A DIVIDER FOR THE CLAIRTY OF OF PRINTING"
(random-note)
(random-note)
(random-note)
(random-note)
(random-note)
(random-note)
(random-note)
(random-note)
(random-note)
(random-note)
"---"


;;a chord is (make-chord note note2 note3)
(define-struct chord (note note2 note3))

(chord (midiNote 69 (ms 1000))
       (midiNote 55 223)
       (midiNote 10 4410))
(chord (midiNote 69 (ms 1000))
       (noSound 56)
       (midiNote 10 4410))
(chord (noSound 987)
       (noSound 56)
       (noSound 987))


;; random-chord: void -> chord
 ; produces a random set of 3 notes
(define (random-chord)
  (chord (random-note) 
         (random-note) 
         (random-note)))

;; chord->list chord -> list
 ; given a chord, produces a list of its notes
(define (chord->list chord)
  (list (chord-note chord)
        (chord-note2 chord)
        (chord-note3 chord)))

(play (notes->sound (chord->list (random-chord))))