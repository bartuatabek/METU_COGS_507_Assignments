;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Graded Assignment #1

;Question #1
;========================
;Write a program that takes a sequence, a start index,
;an end index and returns the sub-sequence from start to (and including) end.
;Remark: In the given example interractions it is shown that when one of the
;inputs is invalid it returns nil. Therefore if start  index is  invalid the
;function will return nil. Also if the list is invalid or empty the function
;will also return nil. If the start index is equal to end index then only the
;element in that index is returned.

;Helper function to copy the list lst from beginning to the element at position end - 1"
(defun copy (lst end)
  (if (zerop end) nil (cons (car lst) (copy (cdr lst) (- end 1)))))

;Helper function that copies the list l from the element at
;position start to the element at position end - 1"
(defun subseq-iter (lst start &optional (end (length lst)))
  (if (zerop start)
      (copy lst end)
      (subseq-iter (cdr lst) (- start 1) (- end 1))))

;Function returns a sublist of the original inputted list with
;the given starting and end positions.
(defun sub-sequence (lst start end)
  (cond ((not (listp lst)) nil)
        ((> start (- (length lst) 1)) nil)
        ((> start end) nil)
        ((> end (- (length lst) 1)) (subseq-iter lst start))
        (t (subseq-iter lst start (+ end 1)))
  )
)

;Question #2
;========================
;Write a program that takes two parameters count and max,
;and returns a list of count random integers, all less than max.
;Remark: I'm not sure whether I've understood the question entirely.
;Therefore, I've implemented for each two possiblity.

;Function that takes two arguments count (n) and and maximum (limit)
;and generated a list of n random numbers all less than the maximum.
(defun filter-max-random (countl maximum)
  (cond ((<= countl 0) nil)
        (t (cons (random maximum) (filter-max-random (- countl 1) maximum)))))

;Function that takes a list of random n integers and a maximum value
;removes all values greater or equal than the max value, returns the
;remaining list.
(defun filter-max (countl maximum)
  (if countl
    (if (< (car countl) maximum)
      (cons (car countl) (filter-max (cdr countl) maximum))
      (filter-max (cdr countl) maximum)
    )
  )
)

;Question #3
;========================
;Write a program that takes a list of integers as input and return the
;increasing subsequence with the largest sum. An increasing subsequence is
;such that every element of it is either the last element or is less than or
;equal to the element coming after it. Return the last found subsequence in case of equality.
;Remark: Some of the functions used for calculating the increasing subsequence with their sums
;are used both in Question #3 and Question #4. 

;Helper function for checking the validity of the inputted list.
;Function checks whether the list consists of all integers and 
;is not empty.
(defun validate (lst) 
  (cond ((endp lst) t)
        ((not (numberp (car lst))) nil)
        (t (validate (cdr lst)))))

;Helper function to calculate the sum of the inputted list
(defun list-sum (lst)
  (if lst
    (+ (car lst) (list-sum (cdr lst)))
    0))

;Helper function that returns the list with bigger sum
;Returns the last found subsequence in case of equality.
(defun larger-sum (lst1 lst2)
  (if (> (list-sum lst1) (list-sum lst2)) lst1 lst2))

;Helper function that calculates the increasing subsequence of a list
;with the largest sum starting with the inputted index.
;Remark: Increasing subsequence of a list with maximum sum may not
;always means the longest list. In order to check all combinations
;the recursive function is called twice if an element is added to the
;sequence in order to go through other values. Since the functions returns
;the maximum subsequence simulaneously I've used setq to update the max-subseq
;accumulator for keeping the sequence until all the recursive calls returns.
(defun max-subsequence (lst sequence &optional max-subseq prev)
  (cond ((endp lst) max-subseq)
        ((>= (car lst) (car (reverse sequence))) (setq max-subseq (larger-sum (max-subsequence (cdr lst)
                                                                              (reverse (cons (car lst) (reverse sequence)))
                                                                              max-subseq
                                                                              sequence)
                                                                              (max-subsequence (cdr lst) sequence max-subseq sequence))))
        (t (max-subsequence (cdr lst) sequence (larger-sum sequence prev) sequence))))

;Helper function for iterating each element in the list and calculate
;all their increasing subsequences by calling increasing-subsequence.
(defun max-increasing-subsequence (lst &optional max-subseq)
  (cond ((not (validate lst)) "Error: Invalid Input")
        ((endp lst) max-subseq)
        (t (max-increasing-subsequence (cdr lst) max-subseq) 
           (max-subsequence (cdr lst) (list (car lst)) max-subseq))))

;Question #4
;========================
;Write a program that first generates a random integer list of size 100,
;prints it, and prints all of its increasing subsequences together with their sums.

;Helper function that generates a random integer list of size 100.
;Remark: Since the range of random numbers is not specified
;I've set it as 100 but it can overriden by changing the limit parameter.
(defun generate-random-lst ( &optional (limit 100) (countl 100))
  (cond ((<= countl 0) nil)
        (t (cons (random limit) (generate-random-lst limit (- countl 1))))))

;Helper function for printing the inputted list along with its sums.
;Remark: Since the printing format is not exclusively specified I've
;printed the increasing subsequences along with their sums by embedding
;all of them inside a list as shown in the example ((1 2 3 4) SUM -> 10)
(defun print-sum (lst)
  (list lst 'sum '-> (list-sum lst))
)

;Helper function that calculates all the increasing subsequences of the
;given list starting with the inputted element, prints it along with its sum.
;For example, (increasing-subsequence (1 13 7 0) (4)) it will print the following
;(4) ;(4 13) ;(4 7)
;Remark: The function iterates over all the list elements twice checking
;for all the possible increasing subsequences. If it finds an element greater or equal
;then the last element of the sequence it appends it to its back and continuing iterating
;until the list reaches the end. However, for these cases it also skips the mathed item
;for checking other possibilities. For example for the input (4 1 13 7 0) after finding
;the subsequence (4 13) it skips 13 and continues iterating in order to include the sequence (4 7)
;Remark #2: In order to execute two tasks back to back; i.e. printing the current subsequence
;and the recursion part I've had to use the progn function. Since its usage isn't explicitly stated
;as not allowed I've used it only for this task.
(defun increasing-subsequence (lst sequence &optional prev)
  (progn
    (if (not (equal sequence prev)) (print (print-sum sequence)))
    (cond ((endp lst) t)
          ((>= (car lst) (car (reverse sequence))) (increasing-subsequence (cdr lst)
                                                                           (reverse (cons (car lst) (reverse sequence)))
                                                                           sequence)
                                                   (increasing-subsequence (cdr lst) sequence sequence))
          (t (increasing-subsequence (cdr lst) sequence sequence))
    )
  )
)

;Helper function for iterating each element in the list and calculate
;all their increasing subsequences by calling increasing-subsequence.
(defun random-increasing-subsequence-iterator (lst)
  (cond ((endp lst) t)
        (t (random-increasing-subsequence-iterator (cdr lst)) 
           (increasing-subsequence (cdr lst) (list (car lst)))
        )
  )
)

;This function first generates a random list with 100 elements and
;assigns it as random-lst. Then it prints the contents of the list.
;Then calls the random-increasing-subsequence-iterator function for
;calculating and printing all of the increasing subsequences with sums.
;========================
;Critical Remark: Printing all the increasing subsequences of a random
;list with 100 elements means that there is approximately 100! possible
;increasing sequences and therefore its not efficient nor logical to
;run this function with 100 elements because of the runtime complexity.
;For testing purposes I recommend filling the optional parameters of the
;function with reasonable numbers in order to see the correctness of the code. 
(defun random-increasing-subsequence (&optional (limit 100) (count 100))
  (let ((random-lst (generate-random-lst limit count)))
        (print "Random Generated List")
        (print random-lst)
        (print "All Increasing Subsequences With Their Sums")
        (random-increasing-subsequence-iterator random-lst)
  )
)