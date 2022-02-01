;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #12

;Exercise 8.1
;========================
;Define a procedure APPEND2 that appends two lists.
(defun append2 (lst1 lst2)
  (dolist (x (reverse lst1) lst2) 
    (push x lst2)))

;Exercise 8.2
;========================
;Define an iterative procedure CHOP-LAST, which removes
;the final element of the given list.
(defun chop-last (lst)
 (let ((store nil)
 			 (counter 0)
			 (last-index (- (length lst) 1)))
	(dolist (x lst (reverse store)) 
		(if (not (= counter last-index))
			(progn
				(incf counter)
				(setf store (cons x store)))))))

;Exercise 8.3
;========================
;Define an iterative procedure UNIQ that takes a list
;and removes all the repeated elements in the list
;keeping only the first occurrence.
(defun uniq (lst)
  (let ((result nil))
		(dolist (x lst (reverse result))
	  (if (not (member x result))
			(push x result)))))

;Exercise 8.4
;========================
;Define a procedure that reverses the top-level elements of a list.
;Remark: I interpreted top-level elements as if there are any sublists
;inside the input list they won't be reversed.
(defun reverse-top-level (lst)
  (let ((result nil))
	(dolist (x lst result)
		(push x result))))

;Exercise 8.5
;========================
;Implement RUN-MEAN by using DOTIMES and NTH.
(defun run-mean (lst)
  (let ((mean nil)
				(sum 0))
		(dotimes (i (length lst) (reverse mean))
	  	(incf sum (nth i lst))  
	  	(push (/ sum (incf i)) mean))))

;Exercise 8.6
;========================
;Define a procedure SEARCH-POS that takes a list as search item,
;another list as a search list and returns the list of positions
;that the search item is found in the search list.
;Remark: I searched the overall subsequences one by one in the size of search-item
;and if it was equal to the current index I added it to the accumulator.
(defun search-pos (search-item search-list)
  (let ((start 0)
				(indexes nil)
				(size (length search-item))
				(list-size (length search-list)))
				(dotimes (i list-size (reverse indexes))
					(if (or (>= start list-size) (>= (+ size start) list-size)) 
						(return (reverse indexes))
					)
					(incf start)
					(if (equal (subseq search-list start (+ start size)) search-item)
						(push start indexes)))))

;Exercise 8.7
;========================
;Define a procedure that reverses the elements
;in a list including its sublists as well.
(defun reverse-all (list-of-lists)
  (let ((result nil))
	(dolist (lst list-of-lists result)
		(if (listp lst)
			(push (reverse-all lst) result)
		(push lst result)))))

;Exercise 8.8
;========================
;Write a procedure LAST-NTH that returns the nth
;element from the end of a given list.
(defun last-nth (n lst)
	(let ((counter 0)
				(index (- (length lst) n)))
  	(dolist (i lst)
			(if (= counter index) 
				(return i)
			(incf counter)))))

;Exercise 8.9
;========================
;Define a procedure that “pairs” an arbitrary number of lists.
(defun pairlists (list-of-lists)
  (let ((pair-lst nil)
				(size (length (car list-of-lists))))
		(dotimes (i size (reverse pair-lst))
			(push 
				(let ((sub-pairs nil))
					(dolist (j list-of-lists (reverse sub-pairs))
						(push (nth i j) sub-pairs)))
				pair-lst))))