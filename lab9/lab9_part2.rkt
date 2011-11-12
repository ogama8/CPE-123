;;Eli Backer - Lab 9 part 2 - Clements - CPE 123 - Fall 2011

;; Initial Requires/Defines
#lang racket
(require (planet clements/rsound))
(require (planet "draw.rkt" ("clements" "rsound.plt" 2 5)))
(define (ms x) (floor (* x 441 1/10)))

;; note [number or booleean] number number -> rsound
 ; Given MIDI number, attack (ms), release (ms) and length (ms) -> tone at pitch with attributes listed
 ; Given false -> 1/4 sec silence
(define (note nn att rel length)
  (cond ([false? nn] (silence (ms length)))
        (else (local [(define (tone n) 
                        (cond ([< n (ms att)] (* (/ n (ms att)) 
                                                 (sin (* 2 n pi 1/44100 (* 440 (expt 2 (/ (- nn 69) 12)))))))
                              ([< 1 (/ (- (ms length) n) (ms rel))] (sin (* 2 n pi 1/44100 (* 440 (expt 2 (/ (- nn 69) 12))))))
                              (else (* (/ (- (ms length) n) (ms rel)) 
                                       (sin (* 2 n pi 1/44100 (* 440 (expt 2 (/ (- nn 69) 12)))))))))]
                (mono-signal->rsound (ms length) tone)))))


;; notes->song: list list -> rsound
 ; Given list of MIDI notes or false and a list of modifiers, produces that sequence of notes
(define (notes->song noteList1 noteList2)
  (rs-append* (for/list ([nn (in-list noteList1)]
                         [len (in-list noteList2)])
                (note nn 5 5 (* 125 len)))))

;; Note List Edelweiss
#;(define edelweiss (list 61 61 64 71 71 71 69 69 64 62 62 false
                        61 61 61 61 62 64 66 66 66 64 64 false
                        61 61 64 71 71 71 69 69 64 62 62 false
                        61 61 64 64 66 68 69 69 69 69 69 false
                        71 71 64 68 66 64 61 61 64 69 69 false
                        66 66 69 71 71 69 68 68 66 64 64 false
                        61 61 64 71 71 71 69 69 64 62 62 false
                        61 61 64 64 66 68 69 69 69 69 69 false))

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
(rsound-draw (notes->song edelweiss edelweissLength))
