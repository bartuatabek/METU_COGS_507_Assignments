;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #6

;Exercise 4.14
;========================
;Function outputs if the inputted object is a member of the list or not.
;Used Lisp's member function for checking the membership and returned a
;list consisting of symbols as output.
(defun my-member (x lst)
  (if (member x lst)
    (list x 'IS 'A 'MEMBER 'OF lst)
    (list x 'IS 'NOT 'A 'MEMBER 'OF lst))) 

;Exercise 4.15
;========================
;1. Define a procedure SUBSETP that takes two list arguments and
;decides whether the first is a subset of the second.
(defun subset (lst1 lst2)
  (cond ((endp lst1) t)
        ((member (car lst1) lst2) (subset (cdr lst1) lst2))))

;2. Define a procedure EQUIP that takes two list arguments and
;decides whether the two are equivalent.
(defun equip (lst1 lst2)
  (if (and (subset lst1 lst2) (subset lst2 lst1)) t))

;3. Define a procedure IDENP that takes two list arguments and
;decides whether the two have the same elements in the same order.
;Remark since the list can contain members other than numbers I've
;had to check the equality of two list elements and I've used eq procedure.
(defun idenp (lst1 lst2)
  (cond ((/= (length lst1) (length lst2)) nil)
        ((and (endp lst1) (endp lst2)) t)
        ((eq (car lst1) (car lst2)) (idenp (cdr lst1) (cdr lst2)))))

;Exercise 4.16
;========================
;Define a procedure that takes as input employee data and a threshold salary
;(an integer), and returns in a list the last names of all the employees that
;earn above the threshold salary.

;Remark the inputted employee data is imagined as list of pairs; an employee name
;and their salary in that order. i.e. '((adam 1234) (eve 5678))

;with accumulator
(defun salary-acc (employee-data threshold &optional result)
  (cond ((endp employee-data) result)
        ((> (cadar employee-data) threshold) (salary-acc (cdr employee-data) threshold (append result (list (caar employee-data)))))
        (t (salary-acc (cdr employee-data) threshold result))))

;without accumulator
(defun salary (employee-data threshold)
  (if employee-data
    (cond ((> (cadar employee-data) threshold) (cons (caar employee-data) (salary (cdr employee-data) threshold)))
          (t (salary (cdr employee-data) threshold)))))

;Exercise 4.17
;========================
;Using MEMBER and LENGTH, write a function ORDER which gives the order of an item in a list.
;Remark: MEMBER returns the list starting from the selected item. Substracting its length 
;from the lists lenght and adding one will give us the index of the element.
(defun order (element lst)
  (let ((index (+ 1 (- (length lst) (length (member element lst))))))
    (if (member element lst) index)))

;Exercise 4.18
;========================
;Define a procedure that computes the sum of a list of numbers
;Remark: if the list does not contain numbers it returns nil

;with accumulator
(defun list-sum (lst &optional (sum 0))
  (cond ((endp lst) sum)
        ((not (numberp (car lst))) nil) 
        ((list-sum (cdr lst) (+ sum (car lst))))))

;without accumulator
(defun list-sum2 (lst)
  (cond ((endp lst) 0)
        ((not (numberp (car lst))) nil) 
        (t (+ (car lst) (list-sum2 (cdr lst))))))

;Exercise 4.19
;========================
;Define a procedure LISTPRO that returns the
;product of all the numbers in a list
;Remark: In order to keep the results the accumulator
;and the base case had to have 1 as their values.

;with accumulator
(defun listpro (lst &optional (product 1))
  (cond ((endp lst) product)
        ((not (numberp (car lst))) (listpro (cdr lst))) 
        ((listpro (cdr lst) (* product (car lst))))))

;without accumulator
(defun listpro2 (lst)
  (cond ((endp lst) 1)
        ((not (numberp (car lst))) (listpro2 (cdr lst))) 
        (t (* (car lst) (listpro2 (cdr lst))))))

;Exercise 4.20
;========================
;Define a procedure that returns the largest number in a list of numbers.
(defun max-list (lst)
  (if (and (not (endp lst)) (numberp (car lst))) (max-lst (cdr lst) (car lst))))

;Helper function to calculate the maximum value recursively
(defun max-lst (lst maximum)
  (cond ((endp lst) maximum)
        ((not (numberp (car lst))) (max-lst (cdr lst) maximum))
        ((> (car lst) maximum) (max-lst (cdr lst) (car lst)))
        ((max-lst (cdr lst) maximum))))

;Exercise 4.21
;========================
;Define a recursive member procedure that checks whether
;a given item is found in the given list.
(defun member-rec (lst item)
  (cond ((endp lst) nil)
        ((eq (car lst) item) t)
        ((listp (car lst)) (or (member-rec (car lst) item) (member-rec (cdr lst) item)))
        (t (member-rec (cdr lst) item))))

;Exercise 4.22
;========================
;Define a procedure SPLIT that takes an integer n and a list,
;and returns a list of two lists such that the first has the
;first n elements of the input list, and the second has the
;rest of the elements of the input list.
;Remark: For the values of n where n is a negative number or
;n is greater than the size of the list the full list is returned.
;However in the case of where n is equal to the size of list
;it will return two lists where all the elements are in the 
;first list and only an empty list is remaining for the second list.
(defun split (lst n &optional (result))
  (if (= n 0)
    (if (and (= n 0) (endp result)) lst (cons result (list lst)))
    (if (endp lst) result (split (cdr lst) (- n 1) (append result (list (car lst)))))))

;Exercise 4.23
;========================
;Define a procedure SPLIT-TWO that splits a list of numbers
;into two equal length lists. If the original list has an
;odd length, there will be a middle element. Split this middle
;element between the two halves; half of it goes to the end
;of the first list, half of it goes to the beginning of the second list.
;Remark: I've used the same split function from Exercise 4.22 for lists
;with even length. I've passed n as the half size of the length. For the
;odd length lists I've found the middle element and insterted half of it
;to the resulted list from split.
(defun split-two (lst)
  (if (evenp (length lst)) (split lst (/ (length lst) 2))
   (let ((middle-element (nth (floor (length lst) 2) lst))
         (result (split lst (floor (length lst) 2))))
         (list (append (car result) (list (/ middle-element 2))) (append (list (/ middle-element 2)) (cdr (cadr result)))))))
         
;Exercise 4.24
;========================
;Define a procedure that gives the last element of a list or gives NIL if the list is empty.
(defun lastt (lst)
  (cond ((endp lst) nil)
        ((= 1 (length lst)) lst)
        (t (lastt (cdr lst)))))