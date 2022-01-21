'6.10
(define queens
  (lambda (n)
(let ((searcher
  (lambda (legal? solution? fresh-try)
	(letrec
	  ((build-solution
		 (lambda (legal-pl)
		   (display "Build-Solution	: ")
		   (display (reverse legal-pl))
		   (newline)
		   (cond 
			 ((solution? legal-pl) legal-pl)
			 (else (forward fresh-try legal-pl)))))
	   (forward
		 (lambda (try legal-pl)
		   (cond
			 ((zero? try) (backtrack legal-pl))
			 ((legal? try legal-pl)
			  (build-solution (cons try legal-pl)))
			 (else (forward (1- try) legal-pl)))))
	   (backtrack
		 (lambda (legal-pl)
		   (display "Backtrack	: ")
		   (display (reverse legal-pl))
		   (newline)
		   (cond
			 ((null? legal-pl) '())
			 (else 
			   (forward (1- (car legal-pl)) (cdr legal-pl))))))
	   (build-all-solutions
		 (lambda ()
		   (letrec
			 ((loop (lambda (sol)
					  (cond
						((null? sol) '())
						(else (cons sol (loop (backtrack sol))))))))
			 (loop (build-solution '()))))))
	  (build-solution '())))))
	(searcher 
	  (lambda (try legal-pl)
		(letrec
		  ((good?
			 (lambda (new-pl up down)
			   (cond
				 ((null? new-pl) #t)
				 (else (let ((next-pos (car new-pl)))
						 (and
						   (not (= next-pos try))
						   (not (= next-pos up))
						   (not (= next-pos down))
						   (good? (cdr new-pl)
								  (1+ up)
								  (1- down)))))))))
		  (good? legal-pl (1+ try) (1- try))))
	  (lambda (legal-pl)
		(= (length legal-pl) n))
	  n))))
  
(queens 8)

