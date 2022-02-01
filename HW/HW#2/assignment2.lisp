;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #2

;Exercise 2.1
;========================
;Procedure takes two numbers x & y
;calculates their difference (x-y)
;returns -1 if (x-y) < 0
;          0 if (x-y) = 0
;          1 if (x-y) > 0
(defun procedure (x y)
  (let ((diff (- x y)))
    (cond ((< diff 0) -1)
          ((= diff 0) 0)
          ((> diff 0) 1))))
  

;Exercise 2.2
;========================
;Same functionality as the procedure above
;with the addition of numberhood validation
;returns 'NIL' if x | y is not numbers
(defun procedure2 (x y)
  (if (numberp x)
    (if (numberp y) 
      (let ((diff (- x y)))
        (cond ((< diff 0) -1)
              ((= diff 0) 0)
              ((> diff 0) 1)))
      nil)
    nil))

;Exercise 2.3
;========================
;Procedure takes three numbers and returns T
;if all three are integers and returns NIL otherwise
(defun procedure3 (x y z)
  (if (numberp x)
    (if (numberp y) 
      (if (numberp z)
        T
        nil) 
      nil)
    nil))

;Exercise 2.4
;========================
;Takes three numbers x,y,z and returns
;'ADDED'      if (x + y) = z 
;'MULTIPLIED' if (x * y) = z 
;'DIVIDED'    if (x / y) = z 
;'SUBTRACTED' if (x - y) = z 
;'DONT-KNOW'  if none of the above 
;Note: Some inputs i.e. (1,0,1) may applicable for multiple results
;(for example (1 + 0) = 1 & (1 - 0) = 1) first suitable operation is 
;returned since otherwise not stated.
(defun HOWCOMPUTE (x y z)
  (cond ((= (+ x y) z) 'ADDED)
        ((= (* x y) z) 'MULTIPLIED)
        ((= (/ x y) z) 'DIVIDED)
        ((= (- x y) z) 'SUBTRACTED)
        ('DONT-KNOW)))

;Exercise 2.5
;========================
;Function takes two arguments and returns the greater of the two.
;In case any of the arguments is not a number an error symbol with
;description is returned
(defun greater (x y)
  (if (and (numberp x) (numberp y))
    (if (>= x y)
      x
      y)
    (defvar INPUT-NOT-NUMBER)))

;Exercise 2.6
;======================== 
;Function takes three arguments and returns the greater of the two.
;In case any of the arguments is not a number an error symbol with
;description is returned
(defun greatest (x y z)
  (if (and (numberp x) (numberp y) (numberp z))
    (cond ((and (>= x y) (>= x z)) x)
          ((and (>= y x) (>= y z)) y)
          (z))
    (defvar INPUT-NOT-NUMBER)))

;Exercise 2.7
;======================== 
;Function takes three numbers x,y,z and gives back the second 
;largest of them using only if and comparison predicates 
(defun second-largest (x y z)
    (if (<= x y)
      (if (<= y z)
        y
        (if (< x z)
          z
          x))
      (if (<= x z)
        x
        (if (> z y)
          z
          y))))

;Exercise 2.8
;======================== 
; Helper function that returns the square of n
(defun square (n) (* n n))

;Helper function calculates the sum of the squares of three numbers
(defun sum-square (x y z)
  (+ (square x) (square y) (square z)))

;Function takes three numbers and gives back the sum of the
;squares of the larger two by subtracting the square of the minumum
(defun max-sum (x y z)
  (- (sum-square x y z) (square (min x y z))))

;Exercise 2.9
;======================== 
;Function takes three numbers x,y,z and gives
;back the second largest of them using COND 
(defun second-largest-cond (a b c)
  (cond ((and (> a b) (> c a)) a)
        ((and (> a b) (> b c)) b)
        ((and (<= a b) (> c b)) b)
        ((and (<= a b) (> a c)) a)
        (t c))
)

;Exercise 2.10
;======================== 
;Function halves a given number until the result becomes
;less than 1 and returns that result using recursion
(defun halver (n)
  (cond ((< n 1) n)
        ((halver (/ n 2)))))

;Exercise 2.11
;======================== 
;Rewrite (AND X Y Z W) by using cond COND by
;intertwining the COND statements of each symbol:

;(cond (X (cond (Y (cond (Z (cond (W))))))))

;Exercise 2.12
;======================== 
;Write COND statements equivalent to 
;(NOT U):
;(cond (U nil)
;      (t))

;(OR X Y Z):
;(cond (X) (Y) (Z)) 

;Exercise 2.13
;======================== 
;Final version of the CHANGE-COND program using
;only AND and OR, no IF, no COND
(defun CHANGE-COND (n)
  (and (numberp n)
    (or 
      (and (integerp n) 
        (or
          (and (evenp n) (/ n 2))
          (and (zerop (mod n 3)) (+ (* 3 n) 1))
          n))
      (CHANGE-COND (round n)))))

;Exercise 2.14
;======================== 
; (defun custom-if (test succ fail) ; wrong!
;   (or (and test succ) fail))

;The faulty part in the definition above is that if a
;conditional statement is entered in the success parameter
;the operation (and test succ) will return NIL if succ evaluates
;to NIL therefore resulting in the return of fail irregardless of
;the evaluation of test symbol. For example, the stated call 
;(custom-if T (evenp 1) -1) will return -1 although it was supposed
;to return NIL.

;By adding an extra and statement for validating whether test
;and fail symbols are not nil i.e. OR'ing the following the conditions
;in order to mimic the if else statement:
;test and succ are not NIL (if case)
;not test and fail are not NIL (else case)
(defun custom-if (test succ fail)
  (or (and test succ) (and (not test) fail)))

