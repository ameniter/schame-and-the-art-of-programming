(define nextline
  (lambda (line-ls)
	(letrec ((helper (lambda (next-ls line-ls)
					  (if (null? (cdr line-ls))
						(cons 1 next-ls)
						(helper (cons (+ (car line-ls) (cadr line-ls)) next-ls) 
								  (cdr line-ls))))))
	  (helper '(1) line-ls))))


;(nextline '(1 1))

(define m-thline
  (lambda (m)
	(letrec ((helper 
			   (lambda (m line-ls)
				 (if (= m 0)
				   line-ls
				   (helper (1- m) (nextline line-ls))))))
	  (helper m '(1)))))

(m-thline 4)

(define binomial
  (lambda (m n)
	(list-ref (m-thline m) n)))

(binomial 4 0)




