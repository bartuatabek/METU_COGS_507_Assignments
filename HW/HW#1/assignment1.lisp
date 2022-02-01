; Exercise 1.1
; The following terminal commands were used
; in order to complete Exercise 1.1

; %mkdir lisp
; %cd lisp
; %emacs test.lisp

; On another terminal window following sequence followed:

; %cd lisp
; %rlwrap sbcl
; * (load "test.lisp")
;========================
; %mv test.lisp area.lisp
;========================
; * (load "area.lisp")

; Exercise 1.2
(defun POWER (x y)
            (if (= y 0) 1
                (* x (POWER x (- y 1)))))

(defun OPERATION (x y n)
            (* (/ (POWER x n) (- 7 (/ y 2)))
               (/ (+ (POWER y (/ 2 3)) 17) 4)))

; Exercise 1.3
(defvar x 8) ; x = 8
(defvar y) ; y is defined but unbound
(defvar z) ; z is defined but unbound

(setf y (setf z (* x 2)))
; First z is set as x*2 which is 16
; Then y is set as the same value of z (y=z) which is also 16.

; x = 8 | y = 16 | z = 16

; Exercise 1.4
; Takes two numbers as input and calculates
; their average by taking their sum and dividing
; it by two
(defun average (x y)
    (if (and (numberp x) (numberp y))
        (/ (+ x y) 2)
     nil))

; Exercise 1.5
; Converts the given temperature in Fahrenheit
; to its Celsius equivalent
(defun fahrenheit-to-celsius (temperature)
      (if (numberp temperature)
          (* (- temperature 32) (/ 5 9))
       nil))
