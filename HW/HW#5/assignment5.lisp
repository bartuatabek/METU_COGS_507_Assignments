;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #5

;Exercise 4.1
;========================
;Construct the lists formed by the below expressions,
;using only CONS, elements, and NIL.

;(list 'a 'b 'c)
(cons 'a (cons 'b (cons 'c nil)))

;(list 'a 'b NIL)
(cons 'a (cons 'b (cons nil nil)))

;Exercise 4.2
;========================
;Construct the lists below by using only CONS, elements, and NIL.

;(a b x d)
(cons 'a (cons 'b (cons 'x (cons 'd nil))))

;(a (b (x d)))
(cons 'a (cons '(b (x d)) nil))

;(a (b (d) x))
(cons 'a (cons '(b (d) x) nil))

;(((a (b (x) d))))
(cons '((a (b (x) d))) nil)

;Exercise 4.3
;========================
;Give the sequences of car’s and cdr’s needed to
;get x in the following expressions.

;(a x b d)
(car (cdr lst))

;(a b x d)
(car (cdr (cdr lst)))

;(a (b (x d)))
(car (car (cdr (car (cdr lst)))))

;(a (b (d) x))
(car (cdr (cdr (car (cdr lst)))))

;(((a (b (x) d))))
(car (car (cdr (car (cdr (car (car lst)))))))

;Exercise 4.4
;========================
;Given the list ((A B) (C D) (E F)),
;1. Write what you would get from it by applying the following in order
;Remark: I assumed the statement "in order" as from left to right i.e. for 
;"CAR CDR CDR" first CAR then CDR and so on.

;(a) CAR => (A B)
;(b) CDR CDR ==> ((E F))
;(c) CAR CDR ==> (B)
;(d) CDR CAR ==> (C D)
;(e) CDR CDR CAR ==> (E F)
;(f) CDR CAR CDR CDR ==> NIL or ()

;2. Which sequences of CARs and CDRs would get you A, B and F?
;Remark: I assumed that "geting A, B and F" means getting A,B and F individually.

;CAR CAR ==> A
;CAR CDR CAR ==> B
;CDR CDR CAR CDR CAR ==> F

;Exercise 4.5
;========================
;Write down what the following expressions evaluate to:

;1. (cons 2) ==> error
;2. (cons 2 NIL) ==> (2)
;3. (cons 3 '(2)) ==> (3 2)
;4. (cons 3 (2)) ==> error
;5. (cons NIL NIL) ==> (NIL)
;6. (cons (1 2) NIL) ==> error
;7. (cons '(1 2) NIL) ==> ((1 2))
;8. (cons (A B) NIL) ==> error
;9. (cons ('A 'B) NIL) ==> error
;10. (cons '(A B) NIL) ==> ((A B))
;11. (cons '(A B) '(C D)) ==> ((A B) C D)
;12. (list 1 4) ==> (1 4)
;13. (list 1 '4) ==> (1 4)
;14. (list '1 4) ==> (1 4)
;15. (list 'A B) ==> If symbol B defined before (A (value of B)) else error
;16. (list 'A 4) ==> (A 4)
;17. (list 'A 'B) ==> (A B)
;18. ('list 1 4) == error
;19. (+ 3' 4) ==> 7
;20. ('+ 1 4) ==> error
;21. (list 3 'times '(- 5 2) 'is 9) ==> (3 TIMES (- 5 2) IS 9)
;22. (list 3 'times (- 5 2) 'is '9) ==> (3 TIMES 3 IS 9)

;Exercise 4.6
;========================
;Write down what the following expressions evaluate to.

;1. (if (listp '(list 1 2)) 'ok 'not-really) ==> OK
;2. (if (null (nil)) 'vice 'versa) ==> error
;3. (and (listp (if (> 2 4) (- 2 4) (+ 2 4))) (if (> 2 4) (- 2 4) (+ 2 4))) ==> NIL
;    AND | NIL (6 is not list type) |         |           6               |
;4. (or (listp (if (> 2 4) (- 2 4) (+ 2 4))) (if (> 2 4) (- 2 4) (+ 2 4))) ==> 6
;    OR | NIL (6 is not list type) |         |           6               |
;5. (or (and (or 'or) 'and) 'or) ==> AND
;            |  OR  |
;       |      AND        |
 
;Exercise 4.7
;========================
;Define a procedure that takes a list and an object,
;and returns a list where the object is added to the end of the list.
(defun add-to-list (lst x)
  (append lst (list x)))

;Exercise 4.8
;========================
;Using CAR and CDR, define a procedure to return the fourth element of a list.
(defun fourth-element (lst)
  (car (cdr (cdr (cdr lst)))))

;Exercise 4.9
;========================
;Define a procedure named INSERT-2ND, which takes a list and an object,
;and gives back a list where the element is inserted after the first element of the given list.
;Remark; For empty lists it's not possible to insert at second place so a list with single element
;is returned instead.
(defun insert-2nd (x lst)
  (if (endp lst)
    (list x)
    (append (list (car lst)) (list x) (cdr lst)))) 

;Exercise 4.10
;========================
;Define a procedure named REPLACE-2ND, which is like INSERT-2ND,
;but replaces the element at the 2nd position.
;Remark: If list size is < 2 a new list with element x is returned.
(defun replace-2nd (x lst)
  (if (< (length lst) 2)
    (list x)
    (append (list (car lst)) (list x) (cdr (cdr lst))))) 

;Exercise 4.11
;========================
;Define a procedure SWAP, that takes a two element list and
;switches the order of the elements.
;Remark: If list has less than 2 elements the list itself is returned.
(defun swap (lst)
  (cond ((< (length lst) 2) lst)
        (t (cons (car (cdr lst)) (cons (car lst) nil)))))

;Exercise 4.12
;========================
;Define a procedure AFTER-FIRST that takes two lists and
;inserts all the elements in the second list after the first element of the first list.
(defun after-first (lst1 lst2)
  (append (list (car lst1)) lst2 (cdr lst1)))

;Exercise 4.13
;========================
;The procedure WRAP-2 takes a two element list, and wraps each element inside a list:
;Write two versions of WRAP-2:

;1. one using LIST, CAR and CADR
(defun wrap-2 (lst)
  (list (list (car lst)) (list (cadr lst))))

;2. the other using CONS, CAR and CADR
(defun wrap-2-alternative (lst)
  (cons (cons (car lst) nil) (cons (cons (cadr lst) nil) nil)))
