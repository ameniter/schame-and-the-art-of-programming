(define show-move (lambda (s d)
					 (display s)
					 (display "--> ")
					 (display d)
					 (newline)))

(define hanoik 
  (lambda (n pegs)
	(let ((p1 (car pegs)) 
		  (p2 (car (cdr pegs)))
		  (p3 (car (cdr (cdr pegs))))
		  (rest (cdr (cdr (cdr pegs))))) 
	  (if (> n 0)
		(if (null? rest)
		  (begin
			(hanoik (1- n) 
					(cons p1 (cons p3 (cons p2 rest))))
			(show-move p1 p2)
			(hanoik (1- n) 
					(cons p3 (cons p2 (cons p1 rest)))))
		  (let ((k (quotient n 2)))
			(hanoik k 
					(cons p1 (cons p3 (cons p2 rest))))
			(hanoik (- n k) 
					(cons p1 (cons p2 rest)))
			(hanoik k 
					(cons p3 (cons p2 (cons p1 rest))))))))))

(hanoik 3 '(A B C D))

(define Hanoik 
  (lambda (n pegs)
	(let ((p1 (car pegs)) 
		  (p2 (car (cdr pegs)))
		  (p3 (car (cdr (cdr pegs))))
		  (rest (cdr (cdr (cdr pegs))))) 
	  (if (> n 0)
		(if (null? rest)
		  (append 
			(Hanoik (1- n) 
					(cons p1 (cons p3 (cons p2 rest))))
			(cons (list p1 p2)
				  (Hanoik (1- n) 
						  (cons p3 (cons p2 (cons p1 rest))))))
		  (let ((k (quotient n 2)))
			(append
			  (Hanoik k 
					  (cons p1 (cons p3 (cons p2 rest))))
			  (Hanoik (- n k) 
					  (cons p1 (cons p2 rest)))
			  (Hanoik k 
					  (cons p3 (cons p2 (cons p1 rest)))))))
		'()))))

(Hanoik 3 '(A B C D))
