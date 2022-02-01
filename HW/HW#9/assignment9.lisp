;COGS 502 Symbols and Programming
;Bartu Atabek - 2594679
;Assignment #9

;Sample procedures for testing
(defun f (x) (expt x 2))
(defun g (x) (expt x 3))
(defun h (x) (log x))

;Exercise 6.1
;========================
;Define a procedure VALS that takes a list of one argument
;procedures and an argument, and returns the values obtained
;by applying the procedures to the argument in the given order.
(defun vals (funcs arg) 
  (if (endp funcs) nil
      (cons (funcall (car funcs) arg)
            (vals (cdr funcs) arg))))

;Exercise 6.2
;========================
;Define a procedure PAIRVALS that takes a list of one argument procedures
;and an argument, and returns the list of dotted pairs where each
;procedure is paired with the value obtained by applying it to the argument.
(defun pairvals (funcs arg)
  (if (endp funcs) nil
      (cons (cons (car funcs) (funcall (car funcs) arg))
            (pairvals (cdr funcs) arg))))

;Exercise 6.3
;========================
;Define a procedure MAXPAIR that takes a list of dotted pairs and returns
;the maxi- mum pair where the comparison is done on the basis of the
;second components of pairs.
(defun maxpair (lst &optional max-pair)
	(cond ((endp lst) max-pair)
		((> (cdar lst) (or (cdr max-pair) (- (cdar lst) 1))) (maxpair (cdr lst) (car lst)))
		(t (maxpair (cdr lst) max-pair))))

;Exercise 6.4
;========================
;Define a procedure that takes a list of predicate symbols (e.g. CONSP, NUMBERP etc.)
;and an object, and returns the list of predicates that the object satisfies.
(defun foo (predicates obj &optional result)
  (cond ((endp predicates) (reverse result))
				((funcall (car predicates) obj) (foo (cdr predicates) obj (cons (car predicates) result)))
				(t (foo (cdr predicates) obj result))))

;Exercise 6.5
;========================
;Define a procedure that takes a list of predicate symbols (e.g. CONSP, NUMBERP etc.)
;and a list of objects, collects and returns all the objects that answer yes to at
;least one predicate in the predicate list.
;Helper function calls all the predicate functions for the given object
;and returns T if at least one of them is applicable.
(defun pred-check (predicates obj)
	(cond ((endp predicates) nil)
				(t (or (funcall (car predicates) obj)
							 (pred-check (cdr predicates) obj)))))

(defun multi-obj-pred-check (predicates objs &optional result)
	(cond ((endp objs) (reverse result))
				((pred-check predicates (car objs)) (multi-obj-pred-check predicates (cdr objs) (cons (car objs) result)))
				(t (multi-obj-pred-check predicates (cdr objs) result))))
				
;Exercise 6.6
;========================
;Define a procedure that takes a list of one argument numerical procedures
;(define your own and/or use the built-ins you know) and a number, and
;returns the name of the procedure that yields the maximum value when
;applied to the number argument.
;Function reuses the previously defined pairvals function in Exercise 6.2
;in order to apply and find the result of each function for the number n.
;Then it uses the maxpair function in Exercise 6.3 in order to find the
;maximum value among these pairs and the car of the pair will return the func's name.
(defun maxyield (funcs n)
  (car (maxpair (pairvals funcs n))))