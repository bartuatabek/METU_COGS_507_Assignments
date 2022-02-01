;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Graded Assignment #2

;Question #1
;========================
;Make the recursive Fibonacci solution by using memoization.

;Function creates a hash table for memoization with
;fib(0) = 0 and fib(1) = 1 as initial values. Then
;calls the recursive function for the calculation. 
(defun fib (n)
  (let ((memoized-fibs (make-hash-table)))
       (setf (gethash 0 memoized-fibs) 0)
       (setf (gethash 1 memoized-fibs) 1)
       (recursive-fib n memoized-fibs)))

;The recursive fibonacci function when called with a certain integer,
;first checks whether it has computed this Fibonacci number before by
;looking at the hash table, if there's a match it returns the value
;else it enters into recursion and adds the new value to the table.
(defun recursive-fib (n memoized-fibs)
  (cond ((gethash n memoized-fibs) (gethash n memoized-fibs))
        (t (setf (gethash n memoized-fibs) (+ (recursive-fib (- n 1) memoized-fibs) 
                                              (recursive-fib (- n 2) memoized-fibs))))))

;Question #2
;========================
;Write a program that takes as input a set of numbers and
;sorts the numbers with respect to their Collatz lengths 
;in descending order. 

;Remark: The following two functions for calculating the
;collatz length are taken from the lecture notes.
(defun collatz-generate (n) 
	(if (= n 1)
		'(1)
		(cons n (collatz-generate (if (evenp n)
                                (/ n 2)
                                (+ (* n 3) 1))))))

(defun collatz-length (n)
  (- (length (collatz-generate n)) 1))

;Helper function returns the key values from
;the inputted hash table.
(defun hash-table-keys (hash-table)
  (let ((keys nil))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

;Function first calculates the collatz length of each
;element within the input list and uses it as key value
;as it inserts the element in a hash map.
;Afterwards the key values (collatz lenghts) are extracted 
;from the map and sorted in descending order.
;Finally the sorted keys are iterated and their mapped values
;are added to the result list.
(defun collatz-length-sort (lst)
  (let ((collatz-map (make-hash-table))
        (collatz-list nil)
        (result-list nil))
       (mapcar (lambda (x) (setf (gethash (collatz-length x) collatz-map) x)) lst)
       (setf collatz-list (sort (hash-table-keys collatz-map) '>))
       (dolist (x collatz-list (reverse result-list))
        (push (gethash x collatz-map) result-list))))

;Question #3
;========================
;Define a three argument procedure REMOVE-NTH, which 
;removes every nth occurrence of an item from a list.
;You must use the techniques of Section 8.

;This function validates the given input
;and returns error if any input is invalid
;else it calls the remove-nth functions.
(defun remove-nth (lst item n)
  (cond ((not (listp lst)) "Input parameter lst is not a list")
        ((> n (length lst)) "The nth occurence cannot exceed the lenght of the input list")
        (t (remove-nth-iterator lst item n))))

;The main function that iterates the list
;as it holds a counter. If the counter matches
;the nth occurrence of the given item it skips
;(i.e. removes it) from the resulting list. 
(defun remove-nth-iterator (lst item n)
  (let ((counter 0)
        (store nil))
       (dolist (x lst (reverse store))
          (if (equal x item)
            (if (= counter (- n 1))
              (setf counter 0)
             (progn
               (incf counter)
               (push x store)))
          (push x store)))))

;Question #4
;========================
;Define a procedure NTH-LARGE that returns the nth
;largest number in a list of numbers. You must use
;the techniques of Section 8.

;Iterative sorting helper function that sorts
;the inputted list by swapping consecutive elements
;if the preceding is larger. Repeats this operation
;until all numbers are sorted.
(defun list-sort (lst)
  (let ((lst-length (- (length lst) 1)))
       (dotimes (i lst-length lst)
        (dotimes (j (- lst-length i))
          (let ((num1 (nth j lst))
                (num2 (nth (+ j 1) lst)))   
               (if (> num1 num2)             
                (let ((temp num1))
                     (setf (nth j lst) num2)
                     (setf (nth (+ j 1) lst) temp))))))))

;The following function sorts the given list and gets the
;element at index ((length lst) - n) in order to find the
;nth largest item.
(defun nth-largest (lst n)
  (cond ((not (listp lst)) "Input parameter lst is not a list")
        ((<= n 0) "Input parameter n must be a positive number")
        ((> n (length lst)) "Input parameter n cannot exceed the lenght of the input list")
        (t (nth (- (length lst) n) (list-sort lst)))))

;Remark: This is an alternative nth-largest function written
;below which iterates until it reaches at the index ((length lst) - n)
;instead of using the nth function.
(defun nth-largest-alternative (lst n)
  (cond ((not (listp lst)) "Input parameter lst is not a list")
        ((<= n 0) "Input parameter n must be a positive number")
        ((> n (length lst)) "Input parameter n cannot exceed the lenght of the input list")
        (t (let ((sorted-lst (list-sort lst))
                 (index (+ (- (length lst) n) 1))
                 (nth-max 0))
                (dotimes (i index nth-max)
                  (setf nth-max (car sorted-lst))
                  (setf sorted-lst (cdr sorted-lst)))))))