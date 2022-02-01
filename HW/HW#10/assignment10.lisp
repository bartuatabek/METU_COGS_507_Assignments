;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #10

;Exercise 7.1
;========================
;Write LAMBDA expressions that
;returns the greatest of two integers.
(lambda (x y) (if (> x y) x y))

;given two integers, returns T if one or the other divides the other without remainder.
(lambda (x y) (or (zerop (mod x y)) (zerop (mod y x))))

;given a list of integers, returns the mean.
(lambda (lst) (/ (apply '+ lst) (length lst)))

;given a list of integers, returns the sum of their factorials
(defun factorial (n &optional (result 1))
  (cond ((zerop n) result)
    		(t (factorial (- n 1) (* result n)))))

(lambda (lst) (apply '+ (mapcar 'factorial lst)))

;Exercise 7.2
;========================
;Define a procedure PAIR-PROD using MAPCAR and LAMBDA,
;which takes a list of two element lists of integers
;and returns a list of products of these pairs.
(defun pair-prod (lst-of-lst)
	(mapcar (lambda (lst) (* (car lst) (cadr lst))) lst-of-lst))

;Exercise 7.3
;========================
;Define a procedure that takes two lists as input and
;returns the list of their pairwise averages.
(defun pairwise-avg (lst1 lst2)
		(mapcar (lambda (x y) (/ (+ x y) 2)) lst1 lst2))

;Exercise 7.4
;========================
;Define your own REMOVE-IF.
;Remark: I've reused the remove2 function from the 
;previous assignments and modified it so that instead
;of checking if the car of lst is equal to a number it
;validates if it conforms the given predicate if it does
;it is removed from the list.
(defun my-remove-if (predicate lst)
	(if lst
    (if (not (funcall predicate (car lst)))
      (cons (car lst) (my-remove-if predicate (cdr lst)))
      (my-remove-if predicate (cdr lst)))))

;Exercise 7.5
;========================
;Define LENGTH using MAPCAR, LAMBDA, + and APPLY.
(defun my-length (lst)
	(apply '+ (mapcar (lambda (x) (- x (- x 1))) lst)))

;Exercise 7.6
;========================
;Define a procedure that takes an integer n and gives
;a list of n random single digit numbers.
(defun random-list (n)
	(mapcar (lambda (x) (random 10)) (make-list n)))

;Exercise 7.7
;========================
;Define a procedure that takes two lists: a list N of numbers
;and a list P of symbols with function bindings. Your procedure
;should return a list with the same size as N, whose elements
;are lists consisting of values obtained by applying all the
;procedures in P to the corresponding element in N.

;Sample single input procedures for testing
(defun f (x) (expt x 2))
(defun g (x) (expt x 3))
(defun h (x) (log x))

(defun func-list-mapper (func-lst number-lst)
	(mapcar (lambda (n) (mapcar (lambda (func) (funcall func n)) func-lst)) number-lst))