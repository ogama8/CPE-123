;;Eli Backer - Fall 2011

;; Initial Requires/Defines
#lang racket
(require (planet clements/rsound))
(require (planet "draw.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (floor (* x 441 1/10)))

;; nn->sinInput number -> number
 ; Given MIDI note num. produces function for sine at corresponding freq.
(define (nn->sinInput nn) 
  (* 2 pi 1/44100 (* 440 (expt 2 (/ (- nn 69) 12)))))

;; note [number or booleean] number number number number -> rsound
 ; Given MIDI number, time and optionaly an attack (%time, 0->1), decay (%time, 0->1), sustain vol. (0->1) and release (%time, 0->1) (ms) -> sine tone at pitch with attributes listed
 ; Given false -> 1/4 sec silence
(define defaultAttackValue .5)
(define defaultDecayValue .25)
(define defaultSustainValue .5)
(define defaultReleaseValue .5)

(define (note nn time 
              #:attack [att defaultAttackValue] #:decay [dec defaultDecayValue] #:sustain [sus defaultSustainValue] #:release [rel defaultReleaseValue]
              #:bendTo [nn2 nn])
  (cond ([false? nn] (silence (ms time)))
        (else (local [(define (tone n) 
                        (cond ([< n (ms (* att time))] 
                               (* (/ n 
                                     (ms (* att time))) 
                                  (sin (* n 
                                          (nn->sinInput nn)))))
                              ([< 1 (/ (- (ms time) n) (ms (* rel time)))] (sin (* n 
                                                                                   (nn->sinInput nn))))
                              (else (* (/ (- (ms time) n) (ms (* rel time))) 
                                       (sin (* n (nn->sinInput nn)))))))]
                (mono-signal->rsound (ms (+ time (* time rel))) tone)))))


;; notes->song: list list -> rsound
 ; Given list of MIDI notes or false and a list of modifiers, produces that sequence of notes
(define (notes->song noteList1 noteList2)
  (rs-append* (for/list ([nn (in-list noteList1)]
                         [len (in-list noteList2)])
                (note nn (* 125 len)))))

;; Modified Note List
(define edelweiss (list 61 64 71 69 64 62 false
                        61 61 61 62 64 66 64 false
                        61 64 71 69 64 62 false
                        61 64 64 66 68 69 69 false
                        71 64 64 68 66 64 61 64 69 false
                        66 69 71 69 68 66 64 false
                        61 64 71 69 64 62 false
                        61 64 64 66 68 69 69 false))

(define edelweissLength (list 4 2 6 4 2 4 2
                              4 2 2 2 2 6 4 2
                              4 2 6 4 2 4 2
                              4 2 2 2 2 6 4 2
                              3 1 2 2 2 2 4 2 4 2
                              4 2 4 2 4 2 4 2
                              4 2 6 4 2 4 2
                              4 2 2 2 2 6 4 2))

;;Expect Edelweiss
(play (notes->song edelweiss edelweissLength))

