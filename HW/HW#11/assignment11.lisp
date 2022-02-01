;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #11

;Exercise 7.8
;========================
;Define a procedure APPLIER that takes a procedure proc,
;an input input and a count cnt; and gives the result
;of applying proc to input cnt times. 
(defun applier (proc input cnt)
  (cond ((zerop cnt) input)
    		(t (applier proc (funcall proc input) (- cnt 1)))))

;Exercise 7.9
;========================
;Define a procedure MOST, which takes a list and a procedure argument,
;and returns the element in the list that gives the highest score when
;provided as an argument to the given procedure packed in a list with its score.
(defun most (lst proc)
  (reduce (lambda (x y) (if (> (cadr x) (cadr y)) x y))
        	(mapcar (lambda (x) (list x (funcall proc x))) lst)))

;Exercise 7.10
;========================
;Define your own version of FIND-IF,
;which returns the index together with the element.
(defun find-index-if (proc list &optional (index 0))
 (cond ((endp list) nil)
			 ((funcall proc (car list)) (list index (car list)))
			 (t (find-index-if proc (cdr list) (+ index 1)))))

;Exercise 7.11
;========================
;Define a procedure REPLACE-IF, which takes three arguments:
;a list LST, an item ITEM and a function TEST, and replaces
;every element of LST that passes the TEST with ITEM.
(defun replace-if (lst item test)
  (mapcar (lambda (x) (if (funcall test x) item x)) lst))

;Exercise 7.12
;========================
;Define procedures that use MAPCAR and LAMBDA that
;zip two lists together.
(defun zip (lst1 lst2)
  (mapcar (lambda (x y) (list x y)) lst1 lst2))

;take three lists: first two will be lists of integers,
;and the third is a list of functions. Apply the
;corresponding function to corresponding arguments.
(defun function-mapper (lst1 lst2 funcs)
  (mapcar (lambda (x y func) (funcall func x y)) lst1 lst2 funcs))

;Exercise 7.13
;========================
;Study the following procedure and indicate what the parameters
;n, f, c and s stand for. In other words, describe what this
;procedure computes.
(defun h (n f &optional (c 0) (s 0)) 
	(if (= c n)
		s
		(h n f (+ c (if (= (random 2) f) 1 0)) (+ s 1))))

;This function takes two parameters n & f and tosses a coin while 
;c is not equal to n. If the coin toss result matches with the parameter f
;then c is added one. The number of trials until this condition is met is 
;recorded by parameter s. It is returned when c become equal to n depending
;on the result of coin tosses matching with parameter f.
;Remark: In order for this function to end f should be either 0 or 1.

;Exercise 7.14
;========================
;Find the numbers in a given range that have the same
;Collatz length using the techniques of this section.

;Remark: The following functions for calculating the
;collatz length are taken from the lecture notes.
(defun collatz-generate (n) 
	(if (= n 1)
		'(1)
		(cons n (collatz-generate (if (evenp n)
                                (/ n 2)
                                (+ (* n 3) 1))))))
(defun collatz-length (n)
  (- (length (collatz-generate n)) 1))

;Helper function that creates a list of numbers
;from the given range inputs.
(defun range-list-generator (start end &optional lst)
  (cond ((< end start) lst)
				(t (range-list-generator start (- end 1) (cons end lst)))))

;Helper function that calculates and returns collatz length
;of each element within the input list along with the element.
(defun collatz-length-list (lst)
	(mapcar (lambda (x) (list (collatz-length x) x)) lst))

;Helper function groups and returns the elements with same
;collatz length.
(defun group-collatz-lengths (element collatz-lengths)
	(mapcar (lambda (x) (cadr x))
					(remove-if (lambda (x) (not (= (car x) element))) collatz-lengths)))

;Function generates a list from the given function
;calculates collatz lengths for each element and returns
;the elements with same collatz lengths.
(defun collatz-range (start end &optional lst backup results)
	(cond ((null lst) (collatz-range start end (range-list-generator start end) backup))
				((null backup) (collatz-range start end lst lst))
				((endp lst) results)
				(t (collatz-range start end (cdr lst) backup
													(list results (group-collatz-lengths (car lst) (collatz-length-list lst)))))
	)
)