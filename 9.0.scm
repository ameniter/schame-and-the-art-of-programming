(define vector-tag "vector")

(define vector?
  (lambda (arg)
	(and (pair? arg) (eq? (car arg) vector-tag))))

(define vector-length
  (lambda (vec)
	(car (cdr vec))))

(define vector-ref
  (lambda (vec i)
	((cddr vec) i)))

(define vector-generator
  (lambda (gen-proc)
	(lambda (size)
	  (cons vector-tag (cons size gen-proc)))))
