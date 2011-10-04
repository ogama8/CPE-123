;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Tiny Exercises 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(+ 3 (- 147 13))

(cos (/ (* 3 pi) 2))

(string-length "EliBacker")

(< (string-length "Page") (string-length "Jeffery"))

(require 2htdp/image)
(overlay/xy (ellipse 80 32 "solid" "white") -60 -134
(overlay/xy (ellipse 100 40 "solid" "red") -50 -130 
            (overlay/xy (circle 7 "solid" "blue") -143 -43
                        (overlay/xy (circle 15 "outline" "black") -135 -35 
                                    (overlay/xy (circle 15 "solid" "white") -135 -35
                                                (overlay/xy (circle 7 "solid" "blue") -43 -43
                                                            (overlay/xy (circle 15 "outline" "black") -35 -35 
                                                                        (overlay/xy (circle 15 "solid" "white") -35 -35 (circle 100 "solid" "pink")))))))))
