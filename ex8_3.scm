(load "8_3.scm")

'8.4
(define there-exists
  (lambda (pred)
	(letrec
	  ((test
		 (lambda (s)
		   (and (not (empty-set? s))
				(let ((elem (pick s)))
				  (or (pred elem)
					  (test ((residue elem) s))))))))
	  test)))

;(for-all pred) <=> not (there-exists (not pred))
(define for-all
  (lambda (pred)
	(compose not (there-exists (compose not pred)))))

;(none pred) <=> not (there-exists pred)
(define none
  (lambda (pred)
	(compose not (there-exists pred))))

(define for-all
  (lambda (pred)
	(letrec
	  ((test
		 (lambda (s)
		   (or (empty-set? s)
			   (let ((elem (pick s)))
				 (and (pred elem)
					  (test ((residue elem) s))))))))
	  test)))

;(there-exists pred) <=> not (for-all (not pred))
(define there-exists 
  (lambda (pred)
	(compose not (for-all (compose not pred)))))

;(none pred) <=> for-all (not pred)
(define none
  (lambda (pred)
	(for-all (compose not pred))))

'8.5
(define for-one
  (lambda (pred found-proc not-found-proc)
	(letrec ((test
			   (lambda (s)
				 (if (empty-set? s)
				   (not-found-proc)
				   (let ((v (pick s)))
					 (if (pred v)
					   (found-proc v)
					   (test ((residue v) s))))))))
	  test)))

(define there-exists
  (lambda (pred)
	(for-one pred 
			 (lambda (v) #t)
			 (lambda () #f))))

(define for-all
  (lambda (pred)
	(compose not (for-one (compose not pred)
						  (lambda (v) #t)
						  (lambda () #f)))))

'8.8
(define intersection
  (lambda args
	(if (null? args)
	  the-empty-set
	  (let ((aux
			  (lambda (s1 s2)
				(letrec
				  ((helper
					 (lambda (s1)
					   (if (empty-set? s1)
						 the-empty-set
						 (let ((elem (pick s1)))
						   (if ((contains s2) elem)
							 (adjoin elem (helper ((residue elem) s1)))
							 (helper ((residue elem) s1))))))))
				  (helper s1)))))
		(let ((rest (cdr args)))
		  (if (null? rest)
			(car args)
			(aux (car args) (apply intersection (cdr args)))))))))

(intersection
  (make-set 1 2 3 4)
  (make-set 1 3 4 5)
  (make-set 1 5 6 3 7))

(define union
  (lambda args
	(if (null? args)
	  the-empty-set
	  (let ((aux
			  (lambda (s1 s2)
				(letrec
				  ((helper
					 (lambda (s1)
					   (if (empty-set? s1)
						 s2
						 (let ((elem (pick s1)))
						   (if (not ((contains s2) elem))
							 (adjoin elem (helper ((residue elem) s1)))
							 (helper ((residue elem) s1))))))))
				  (helper s1)))))
		(aux (car args) (apply union (cdr args))))))) 

(define unin
  (lambda args
	(if (null? args)
	  the-empty-set
	  (let ((aux
			  (lambda (s1 s2)
				(letrec
				  ((helper
					 (lambda (s1)
					   (if (empty-set? s1)
						 s2
						 (let ((elem (pick s1)))
						   (if (not ((contains s2) elem))
							 (adjoin elem (helper ((residue elem) s1)))
							 (helper ((residue elem) s1))))))))
				  (helper s1)))))
		(let ((rest (cdr args)))
		  (if (null? rest)
			(car args)
			(aux (car args) (apply unin (cdr args))))))))) 

(unin (make-set 1 2 3 4)
		(make-set 1 3 4 5)
		(make-set 2 1))

(union (make-set 1 2 3 4)
		(make-set 1 3 4 5)
		(make-set 2 1))

(define abstraction
  (lambda (aux)
	(letrec
	  ((helper
		 (lambda args
		   (if (null? args)
			 the-empty-set
			 (let ((rest (cdr args)))
			   (if (null? rest)
				 (car args)
				 (aux (car args) (apply helper (cdr args)))))))))
	  helper)))

((abstraction
			  (lambda (s1 s2)
				(letrec
				  ((helper
					 (lambda (s1)
					   (if (empty-set? s1)
						 s2
						 (let ((elem (pick s1)))
						   (if (not ((contains s2) elem))
							 (adjoin elem (helper ((residue elem) s1)))
							 (helper ((residue elem) s1))))))))
				  (helper s1))))
(make-set 1 2 3 4)
		(make-set 1 3 4 5)
		(make-set 2 1))

((abstraction
   (lambda (s1 s2)
				(letrec
				  ((helper
					 (lambda (s1)
					   (if (empty-set? s1)
						 the-empty-set
						 (let ((elem (pick s1)))
						   (if ((contains s2) elem)
							 (adjoin elem (helper ((residue elem) s1)))
							 (helper ((residue elem) s1))))))))
				  (helper s1)))) 
 (make-set 1 2 3 4)
  (make-set 1 3 4 5)
  (make-set 1 5 6 3 7))

'8.9
;(sym-diff s1 s2) = (s1 U s2) - (s1 ^ s2)
(define symmetric-difference
  (lambda (s1 s2)
	(difference (union s1 s2) (intersection s1 s2))))

(symmetric-difference
  (make-set 1 2 3 4 5)
  (make-set 3 4 5 6 7))

'8.10
;(set-map proc s)
;(adjoin 23 (make-set 1 2 3))
;(set-map (lambda (s)
;		   (adjoin 23 s))
;		 (make-set 
;		   (make-set 1 2 3)
;		   (make-set 3 4 5)))

(let ((s (make-set 1 2 3)))
  (let ((elem (pick s)))
	((residue elem) s)))

(define power-set
  (lambda (s)
	(if (equal? s the-empty-set)
	  (make-set the-empty-set)
	  (let ((elem (pick s)))
		(let ((p1 (power-set ((residue elem) s))))
		  (union p1
				 (set-map 
				   (lambda (set)
					 (adjoin elem set))
				   p1)))))))

(power-set (make-set 'a 'b 'c))

'8.11
;(set-builder pred base-set)
(define select-by-cardinal
  (lambda (int)
	(lambda (s)
	  ((set-builder
		(lambda (set)
		  (eq? (cardinal set) int))
		the-empty-set) s))))


((set-builder (lambda (set)
			   (eq? (cardinal set) 2))
			 the-empty-set)
			 (make-set (make-set 'a 'b) (make-set 'a)))
					   

((select-by-cardinal 2)
 (make-set (make-set 'a) (make-set 'a 'b) (make-set 'a 'b 'c)
		   (make-set 'b 'c) (make-set 'b)))

