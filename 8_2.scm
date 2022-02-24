'8.1
(define neither
  (lambda (pred)
	(both (compose not pred))))

((neither null?) '(a b c) '(d e))

(define at-least-one
  (lambda (pred)
	(compose not (both (compose not pred)))))

((at-least-one even?) 2 3) 

'8.2
(define neither
  (lambda (pred)
	(compose not (at-least-one pred))))

((neither even?) 1 3)

(define ambos 
  (lambda (pred)
	(compose not (at-least-one (compose not pred)))))

((ambos even?) 3 2)

'8.3
(define equal?
  (lambda (arg1 arg2)
	(or (and ((neither pair?) arg1 arg2)
			 (eqv? arg1 arg2))
		(and ((both pair?) arg1 arg2)
			 (equal? (car arg1) (car arg2))
			 (equal? (cdr arg1) (cdr arg2))))))

(equal? '(a b) '(a b))
(equal? '(a (b c (d e) f)) ' (a (b c (d e) f)))
(equal? '(a ((b d) c) e) '(a (b d) c e))
(equal? '(a ((b d) c) e) '(a ((d b) c) e))
