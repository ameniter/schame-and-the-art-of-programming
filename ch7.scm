(define add
  (letrec ((list-add
			 (lambda (ls)
			   (if (null? ls)
				 0
				 (+ (car ls) (list-add (cdr ls)))))))
	(lambda args
	  (list-add args))))

(add 1 2)

(define list (lambda args args))
(list 1 2 3)

(define writeln
  (lambda args
	(for-each display args)
	(newline)))

(writeln 1 "a" 2)

(define add
  (lambda args
	(if (null? args)
	  0
	  (+ (car args) (apply add (cdr args))))))

(define compose
  (lambda (f g)
	(lambda (x)
	  (f (g x)))))

(define h (compose sqrt 1+))

(h 3)

(define plus
  (lambda (x y)
	(if (zero? y)
	  x
	  (1+ (plus x (1- y))))))

(define times
  (lambda (x y)
	(if (zero? y)
	  0
	  (plus x (times x (1- y))))))

(define exponent
  (lambda (x y)
	(if (zero? y)
	  1
	  (times x (exponent x (1- y))))))

(define super
  (lambda (x y)
	(if (zero? y)
	  1
	  (exponent x (super x (1- y))))))

(super 3 2)

(define super-order
  (lambda (n)
	(cond
	  ((= n 1) plus)
	  ((= n 2) times)
	  (else (lambda (x y)
			  (cond
				((zero? y) 1)
				(else ((super-order (1- n))
					   x
					   ((super-order n) x (1- y))))))))))

((super-order 2) 3 4)

(define ackermann
  (lambda (n)
	((super-order n) n n)))

(ackermann 3) 

'7.2
(define compose3
  (lambda (f g h)
	(lambda (x)
	  (f (g (h x))))))

'7.3
(define compose-many
  (lambda args
	(if (null? args)
	  (lambda (x) x)
	  (if (null? (cdr args))
		(car args)
		(compose (car args) (apply compose-many (cdr args)))))))

((compose-many 1+ 1+ 1+ 1+) 3)
((compose-many sqrt abs 1- (lambda (n) (* n n))) 0.6)
(let ((f (lambda (n) (if (even? n) (/ n 2) (1+ n)))))
	  ((compose-many f f f f f f) 21))
((compose-many) 3)

'7.4
(define subtract 
  (lambda (x y)
	(if (zero? y)
	  x
	  (1- (subtract x (1- y))))))

(subtract 3 1)

'7.5
(let ((h (lambda (x) (cons x x))))
  (map h '((1 2) (3 4) (5 6))))

(map (lambda (x) (cons x x)) '((1 2) (3 4) (5 6)))

(map (lambda (x) (+ 5 x)) '(1 2 3 4))

(let ((n 5))
  (let ((proc (lambda (x) (+ n x))))
	(map proc '(1 2 3 4))))

(define iota
  (lambda (n)
	(letrec ((iota-helper
			   (lambda (k acc)
				 (cond
				   ((zero? k) (cons 0 acc))
				   (else (iota-helper (1- k) (cons k acc)))))))
	  (iota-helper (1- n) '()))))

(letrec ((fact
		   (lambda (n)
			 (if (zero? n) 1 (* n (fact (1- n)))))))
  (map fact (iota 6)))

(define mystery
  (lambda (len base)
	(letrec
	  ((mystery-help
		 (lambda (n s)
		   (if (zero? n)
			 (list s)
			 (let ((h (lambda (x)
						(mystery-help (1- n) (cons x s)))))
			   (apply append (map h (iota base))))))))
	  (mystery-help len '()))))

(define mystery-display
  (lambda (len base)
	(letrec
	  ((mystery-help
		 (lambda (n s)
		   (if (zero? n)
			 (writeln s)
			 (let ((h (lambda (x)
						(mystery-help (1- n) (cons x s)))))
			   (for-each h (iota base)))))))
	  (mystery-help len '()))))

(mystery-display 3 3)

(define mystery-explain
  (lambda (len base)
	(letrec
	  ((mystery-help
		 (lambda (n s)
		   (if (zero? n)
			 (list s)
			 (let ((h (lambda (x)
						(mystery-help (1- n) (cons x s)))))
			   ((lambda (ls) 
				  (begin (display "appending ")
						 (for-each display ls)
						 (newline)
						 (apply append ls))) 
				(map h (iota base))))))))
	  (mystery-help len '()))))


(mystery-explain 3 3)


