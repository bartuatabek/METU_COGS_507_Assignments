;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #7

;Exercise 4.26
;========================
;Define a procedure REMOVE2 that takes an element and a list;
;and returns a list where all the occurrences of the
;element are removed from the list.
(defun remove2 (lst element)
  (if lst
    (if (not (equal (car lst) element))
      (cons (car lst) (remove2 (cdr lst) element))
      (remove2 (cdr lst) element))))

;Exercise 4.27
;========================
;Define a procedure REMOVE3 that takes an element and
;a list; and returns a list where all the occurrences
;of the element that are preceded by the symbol X are
;removed from the list.
;Remark I did not use a variable for storing the element
;seen in a previous iteration because it would not contribute
;to the understanding whether the next occurence of x will be final. 

;Helper function that calculates the last index of the given element
(defun last-index (lst element &optional (index 0) last-index)
  (cond ((endp lst) last-index)
        ((eq (car lst) element) (last-index (cdr lst) element (+ 1 index) index))
        (t (last-index (cdr lst) element (+ index 1) last-index))))

;Function removes all instances of the given element except the last
;from the given list
(defun remove3 (lst x &optional (index 0) (backup lst))
  (let ((last-indx (last-index backup x)))
        (cond ((endp lst) nil)
        ((and (equal (car lst) x) (not (= index last-indx))) (remove3 (cdr lst) x (+ index 1) backup))
        (t (cons (car lst) (remove3 (cdr lst) x (+ index 1) backup))))))

;Exercise 4.29
;========================
;Implement RUN-MEAN by using DOTIMES and NTH.
;Remark: Since DOTIMES is not used I've also did not
;use nth and hold various accumulators instead.
(defun run-mean (lst &optional (index 0) (sum 0) mean-lst)
  (cond ((endp lst) (reverse mean-lst))
        (t (run-mean (cdr lst) (+ 1 index) (+ sum (car lst)) (cons (/ (+ sum (car lst)) (+ 1 index)) mean-lst)))))
	
;Exercise 4.30
;========================
;Define a recursive procedure that finds and returns the
;longest chain in a sequence of numbers. If there are more
;than one sequences with the highest length, return the
;one you encountered first. 

;Helper function return the longer sequence,
;returns the first if they have of equal length
(defun longer-sequence (sequence1 sequence2)
  (if (>= (length sequence1) (length sequence2)) sequence1 sequence2))

;Calculates the longest increasing subsequence of a
;given list by iterating over the list from the
;first element and adding a new item to the sequence
;if it is greater or equal than the last element of
;the current sequence.
(defun longest-chain (lst &optional prev seq backup)
  (cond ((endp lst) (if (null backup) seq (longer-sequence backup seq)))
        ((null prev) (longest-chain (cdr lst)
                                    (car lst)
                                    (list (car lst))
                                    backup))
        ((>= (car lst) prev) (longest-chain (cdr lst)
                                            (car lst)
                                            (reverse (cons (car lst) (reverse seq)))
                                            backup))
        (t (longest-chain (cdr lst)
                          (car lst)
                          (list (car lst))
                          (longer-sequence backup seq)))))

;Exercise 4.31
;========================
;Define a procedure that “pairs” an arbitrary number of lists.
;Remark: I've based my function from the pairlists function in
;the lecture notes however without using dotimes I could not
;manage to make it generic for n lists.
;Remark function assumes that lst1 and lst2 have equal
;number of elements inside them.
; (defun pairlists (lst1 lst2 &optional store)
;   (cond ((endp lst1) (reverse store))
;         (t (pairlists (cdr lst1) (cdr lst2) (append store (list (list (car lst1) (car lst2))))))))

(defun pairlists (lst &optional p1 p2)
  (if lst
      (pairlists (cdr lst) (cons (caar lst) p1) (cons (cadar lst) p2))
      (cons (reverse p1) (cons (reverse p2) nil))))

;Exercise 4.32
;========================
;Define a procedure SEARCH-POS that takes a list as search item,
;another list as a search list and returns the list of positions
;that the search item is found in the search list. Positioning starts with 0. 

;Remark: I searched the overall subsequences one by one in the size of search-item
;and if it was equal to the current index I added it to the accumulator.
;Remark #2: Although in the example outputs the indexes were from last to first
;I've reversed the order in order to make it more readable.
(defun search-pos (search-item search-list &optional (start 0) indexes)
  (let ((size (length search-item)))
        (cond ((or (>= start (length search-list)) (>= (+ size start) (length search-list))) (reverse indexes))
              (t (search-pos search-item search-list (+ start 1) (if (equal (subseq search-list start (+ start size)) search-item)
                                                                     (cons start indexes)
                                                                     indexes))))))

;Exercise 4.36
;========================
;Define your own version of NTH (don't use NTHCDR).
;Remark: The input parameter index n starts from 0.
;Returns nil for empty lists or out of bounds.
(defun nth2 (n lst)
  (cond ((zerop n) (car lst))
        (t (nth2 (- n 1) (cdr lst)))))