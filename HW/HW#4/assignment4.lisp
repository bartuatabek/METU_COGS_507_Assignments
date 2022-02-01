;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #4

;Exercise 3.6
;========================
;Define a recursive procedure that takes two integers, say x and y,
;and returns the sum of all the integers in the range including and between x and y.
;Remark: I assumed that x < y in my implementation. In order for the procedure to be
;independent from the order of x,y a helper function to sort x,y could be implemented
;which sorts the input and calls the range-sum accordingly.
(defun range-sum-generic (x y)
  (if (< x y) 
      (range-sum x y)
      (range-sum y x)))

(defun range-sum (x y)
  (cond ((= x y) y)
    (t (+ x (range-sum (+ x 1) y)))))

;Exercise 3.7
;========================
;Define a factorial procedure that uses an accumulator.
(defun factorial (n &optional (acc 1))
  (cond ((zerop n) acc)
        (t (factorial (- n 1) (* n acc )))))

;Exercise 3.8
;========================
;Define a two operand procedure that raises its
;first operand to the power of the second.

;Remark: In order to support negative powers such as 2^(-)3 = 1/8
;I had to use division operation as well since
;the EXPT function also supported it. Is it ok?

;implementation without accumulator
(defun power (x y)
  (cond ((zerop y) 1)
        ((> y 0) (* x (power x (- y 1))))
        (t (* (/ 1 x) (power x (- y -1))))))

;implementation with accumulator
(defun power2 (x y &optional (acc 1))
  (cond ((zerop y) acc)
        ((> y 0) (power2 x (- y 1) (* x acc )))
        (t (power2 x (- y -1) (* (/ 1 x) acc )))))

;Exercise 3.9
;========================
;Define a procedure that gives the Fibonacci number of a given integer.

;implementation without accumulator
(defun fib (n)
  (cond ((< n 2) n)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

;implementation with accumulator
;where a separate counter from n is used
(defun fib2 (n &optional (i 1) (j 0) (counter 2))
  (cond ((< n 2) n)
        ((= counter n) (+ i j))
        (t (fib2 n (+ i j) i (+ counter 1)))))

;implementation with accumulator
;where n itself is used as a counter
(defun fib3 (n &optional (i 1) (j 0))
  (cond ((zerop n) j)
        ((= 1 n) i)
        (t (fib3 (- n 1) (+ i j) i))))

;Exercise 3.10
;========================
;Write a program that computes the square root of a given
;number using Newton's method.

;helper function for taking a square of a number
(defun square (x) (* x x))

;helper function for checking the 2nd step;
;Check if y^2 is close enough to x, the difference is not larger than 0.00001 
(defun close-enough (x y)
  (<= (abs (- x (square y))) 0.00001))

;helper function for updating the guess by replacing y with
;((x/y) + y) / 2
(defun update-guess (x y) (/ (+ (/ x y) y) 2))

;recursive function for calculating the square root of a number using
;Newton's method. The procedure returns the guess if its close enough
;to x. Else it calls itself with an updated guess.
(defun newton-sqrt (x &optional (y 1.0))
  (cond ((close-enough x y) y)
        (t (newton-sqrt x (update-guess x y)))))

;Exercise 3.11
;========================
;Define a recursive procedure that returns the sum of a geometric
;progression with n terms and the initial term a and the common ration r.
;Remark: n is assumed positive number i.e. (> 0).
;Remark #2: For calculating the value of r^i I've used the power procedure
;I've defined in Exercise 3.8.
(defun geometric-progression (a r n &optional (acc 0))
  (cond ((< n 0) acc)
        (t (geometric-progression a r (- n 1) (+ acc (* a (power2 r n)))))))

;Exercise 3.12
;========================
;Define a procedure that takes two arguments n and r, 
;and prints r random numbers between and including 0 and n.
(defun random-numbers (n r)
  (if (> r 0) 
      (and (print (random (+ 1 n))) (random-numbers n (- r 1)))
      t))