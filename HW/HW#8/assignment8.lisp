;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #8

;Exercise 4.44
;========================
;Define a three argument procedure REMOVE-NTH,
;which removes every nth occurrence of an item from a list.
(defun remove-nth (lst item n &optional (occurence 0))
    (cond ((endp lst) nil)
          ((and (equal (car lst) item) 
                (= occurence (- n 1))) (remove-nth (cdr lst) item n 0))            
          (t (cons (car lst) (remove-nth (cdr lst) item n (if (equal (car lst) item) 
                                                              (+ occurence 1) 
                                                              occurence))))))

;Exercise 4.45
;========================
;Define a procedure that takes a list of integers and
;returns the second largest integer in the list.
;Remark: If the list has less than one element then there cannot
;be a second largest number therefore it returns nil.
(defun second-largest (lst &optional (max (car lst)) max-second)
  (cond ((endp lst) max-second)
        ((> (car lst) max) (second-largest (cdr lst) (car lst) max))
        (t (second-largest (cdr lst) max max-second))))

;Exercise 4.46
;========================
;Define a procedure that takes a list of integers
;and an integer n, and returns the nth largest integer in the list.
;Helper function that returns the largest element of the given lst
(defun max-list (lst &optional (max (car lst)))
  (cond ((endp lst) max)
        ((> (car lst) max) (max-list (cdr lst) (car lst)))
        (t (max-list (cdr lst) max))))

;Remark the following function finds the maximum element and removes
;it, n times in order to find the nth largest item.
(defun nth-largest (lst n)
  (cond ((endp lst) nil)
        ((= n 1) (max-list lst))
        (t (nth-largest (- n 1) (remove (max-list lst) lst)))))

;Exercise 4.47
;========================
;Define a procedure UNIQ that takes a list and removes all the
;repeated elements in the list keeping only the first occurrence.
;Don't use REMOVE (built-in or in-house), you may use MEMBER.
(defun uniq (lst &optional uniqlst)
  (cond ((endp lst) uniqlst)
        (t (let ((current (car lst)))
                 (if (member current uniqlst)
                     (uniq (cdr lst) uniqlst)
                     (uniq (cdr lst) (append uniqlst (list current))))))))

;Exercise 4.48
;========================
;Solve Ex 4.47 by keeping the last occurrence rather than the first.
(defun uniq-last (lst)
  (cond ((endp lst) nil)
        (t (let ((current (car lst)))
                (if (member current (cdr lst))
                    (uniq-last (cdr lst))
                    (cons current (uniq-last (cdr lst))))))))

;Exercise 4.49
;Define a procedure REMLAST which removes the last occurrence
;of an item from a list. Do not use MEMBER or REVERSE.
;Remark: If the last occurence of an item is also the first occurence
;i.e. the item occurs only once in the list; it is also removed.
(defun last-index (lst element &optional (index 0) last-index)
  (cond ((endp lst) last-index)
        ((eq (car lst) element) (last-index (cdr lst) element (+ 1 index) index))
        (t (last-index (cdr lst) element (+ index 1) last-index))))

;Function removes all instances of the given element except the last
;from the given list
(defun remlast (lst x &optional (index 0) (backup lst))
  (let ((last-indx (last-index backup x)))
        (cond ((endp lst) nil)
        ((and (equal (car lst) x) (= index last-indx)) (remlast (cdr lst) x (+ index 1) backup))
        (t (cons (car lst) (remlast (cdr lst) x (+ index 1) backup))))))