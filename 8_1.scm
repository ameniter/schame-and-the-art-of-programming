(define compose
  (lambda (f g)
	(lambda args
	  (f (apply g args)))))

((compose 1+ +) 1 2)

(define both
  (lambda (pred)
	(lambda (arg1 arg2)
	  (and (pred arg1) (pred arg2)))))

((both (lambda (ls) (not (null? ls)))) '(a b c) '(d e))

(define neither
  (lambda (pred)
	(lambda (arg1 arg2)
	  (not (or (pred arg1) (pred arg2))))))

((neither null?) '(a b c) '(d e))

(define at-least-one
  (lambda (pred)
	(lambda (arg1 arg2)
	  (or (pred arg1) (pred arg2)))))

