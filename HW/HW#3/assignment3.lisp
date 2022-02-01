;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #3

;Exercise 3.1
;========================
;Define a procedure that multiplies two integers using
;only addition as a primitive arithmetic operation.
;Multiplication of negative numbers are also supported
;by inverting the addition operation i.e. subtraction.
(defun multpl (m n)
  (cond ((or (zerop m) (zerop n)) 0)
        ((< n 0) (+ (- m) (multpl m (+ n 1)))) 
        (t (+ m (multpl m (- n 1))))))

;Exercise 3.2
;========================        
;Define a procedure that computes the factorial of a given integer.
(defun factorial (n)
  (cond ((zerop n) 1)
        (t (* n (factorial (- n 1))))))

;Exercise 3.3
;========================  
;Define a recursive procedure that computes the sum of
;the squares of the first n non-negative integers.
(defun sum-squares (n) 
  (cond ((zerop n) 0)
        (t (+ (* n n) (sum-squares (- n 1))))))

;Exercise 3.4
;========================
;Define a recursive procedure TOSS that takes a non-negative
;integer n, tosses a coin n number of times, printing the
;result (0 or 1) on the screen in each toss.
(defun toss (n)
  (if (> n 0) 
      (and (print (random 2)) (toss (- n 1)))
      t))

;Exercise 3.5
;========================
;Define a procedure COLL that implements the function C:
;     [ 1,           if n = 1
;C(n)=| C(n/2),      if n is even
;     [ C(3n + 1),   if n is odd
(defun coll (n)
  (cond ((= n 1) 1)
        ((evenp n) (coll (/ n 2)))
        (t (print (coll (+ (* n 3) 1))))))
