'6.11
(define legal?
  (lambda (try legal-ls)
	(letrec ((chop-prefix 
			   (lambda (ls prefix-sz)
				 (if (= 0 prefix-sz)
				   ls
				   (chop-prefix (cdr ls) (1- prefix-sz)))))
			 (prefixed?
			   (lambda (ls prefix)
				 (if (null? prefix)
				   #t
				   (if (not (equal? (car ls) (car prefix)))
					 #f
					 (prefixed? (cdr ls) (cdr prefix))))))
			 (get-prefix
			   (lambda (ls size)
				 (if (= size 0)
				   '()
				   (cons (car ls) (get-prefix (cdr ls) (1- size))))))
			 (legal-helper
			   (lambda (size)
				 (if (> (1+ size) (- (length legal-ls) (quotient (length legal-ls) 2)))
				   #t
				 (if (prefixed? (chop-prefix legal-ls  size)
					 (cons try (get-prefix legal-ls size)))
				   #f
				   (legal-helper (1+ size)))))))
	  (legal-helper 0))))


(define good-sequences 
	(let ((searcher
  		(lambda (legal? solution? fresh-try)
			(letrec (
				(build-solution
				  (lambda (legal-ls)
					(display "Build-Solution	: ")
					(display (reverse legal-ls))
					(newline)
					(cond 
					  ((solution? legal-ls) legal-ls)
					  (else (forward fresh-try legal-ls)))))
	   			(forward
				  (lambda (try legal-ls)
					(cond
					  ((zero? try) (backtrack legal-ls))
					  ((legal? try legal-ls)
					   (build-solution (cons try legal-ls)))
					  (else (forward (1- try) legal-ls)))))
				(backtrack
				  (lambda (legal-ls)
					(display "Backtrack	: ")
					(display (reverse legal-ls))
					(newline)
					(cond
					  ((null? legal-ls) '())
					  (else 
						(forward (1- (car legal-ls)) (cdr legal-ls))))))
				(build-all-solutions
				  (lambda ()
					(letrec
					  ((loop (lambda (sol)
							   (cond
								 ((null? sol) '())
								 (else (cons sol (loop (backtrack sol))))))))
					  (loop (build-solution '()))))))
			  (build-all-solutions)))))
;			  (build-solution '())))))
	  (lambda (n)
		(searcher legal? 
				  (lambda (legal-ls)
					(= (length legal-ls) n))
				  3)))) 

(good-sequences 4)
