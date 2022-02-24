(load "8_3.scm")

(define make-op
  (lambda (x y)
	(make-set (make-set x) (make-set x y))))

(define op?
  (lambda (set)
	(and (set? set)
		 ((for-all set?) set)
		 (= (cardinal (family-intersection set)) 1)
		 (or (= (cardinal set) 1)
			 ((both (lambda (x) (= (cardinal x) 2)))
			  set
			  (family-union set))))))

(define op-1st
  (lambda (op)
	(pick (family-intersection op))))

(define op-2nd
  (lambda (op)
	(let ((fam-int (family-intersection op)))
	  (let ((diff (difference (family-union op) fam-int)))
		(pick (if (empty-set? diff) fam-int diff))))))

'8.28
(define make-op
  (lambda (x y)
	(list x y)))

(define op?
  (lambda (ls)
	(and (pair? ls) (pair? (cdr ls)) (null? (cddr ls)))))

(define op-1st
  (lambda (op)
	(car op)))

(define op-2nd
  (lambda (op)
	(cadr op)))

'8.29
(define make-o
  (lambda (x y)
	(cons x y)))

(define o?
  (lambda (pr)
	(pair? pr)))

(define op-1s
  (lambda (op)
	(car op)))

(define op-2n
  (lambda (op)
	(cdr op)))

(define cartesian-product
  (lambda (s1 s2)
	(if (empty-set? s1)
	  the-empty-set
	  (let ((elem (pick s1)))
		(union (set-map (lambda (x) (make-op elem x)) s2)
			   (cartesian-product ((residue elem) s1) s2))))))


(define domain
  (lambda (rel)
	(set-map op-1st rel)))

(define range
  (lambda (rel)
	(set-map op-2nd rel)))

(domain (cartesian-product (make-set 'a 'b 'c) (make-set 'd 'e)))

(define is-older-than-relation
  (make-set (make-op 'tom 'bob)
			(make-op 'tom 'jim)
			(make-op 'bob 'jim)))

(define is-older-than?
  (lambda (b1 b2)
	((contains is-older-than-relation) (make-op b1 b2))))

(is-older-than? 'bob 'tom)

(define subrelation/1st
  (lambda (rel)
	(lambda (arg)
	  ((set-builder
		 (lambda (x) ((set-equal (op-1st x)) arg))
		 the-empty-set)
	   rel))))

((subrelation/1st is-older-than-relation) 'tom)
(difference is-older-than-relation ((subrelation/1st is-older-than-relation) 'tom))


(define function?
  (lambda (rel)
	(or (empty-set? rel)
		(let ((subrel ((subrelation/1st rel) (op-1st (pick rel)))))
		  (and (= (cardinal subrel) 1)
			   (function? (difference rel subrel)))))))

(function? is-older-than-relation)

(define value
  (lambda (fun)
	(lambda (arg)
	  (op-2nd (pick ((subrelation/1st fun) arg))))))

'exercises
'8.13
(define relation?
  (lambda (s)
	(or (empty-set? s)
		(let ((elem (pick s)))
		  (and (op? elem)
			   (relation? ((residue elem) s)))))))

'8.14
(define inverse-relation
  (lambda (rel)
	(set-map 
	  (lambda (op)
		(make-op (op-2nd op) (op-1st op)))
	  rel))) 

is-older-than-relation
(inverse-relation is-older-than-relation)

'8.15
(define one-to-one?
  (lambda (fun)
	(function? (inverse-relation fun))))

(define sample
  (make-set (make-op 'tom 'bob)
			(make-op 'jim 'jim)
			(make-op 'bob 'bob)))

(one-to-one? sample)

'8.16
(define make-relation
  (lambda args
	(if (null? args)
	  the-empty-set
	  (let ((pair (car args)))
		(adjoin (make-op (car pair) (cadr pair))
				(apply make-relation (cdr args)))))))

(make-relation '(1 2) '(1 3) '(2 3))


'8.17
(define reflexive? 
  (lambda (rel)
	(if (equal? rel the-empty-set)
	  #t
	  (let ((s (union (domain rel) (range rel))))
		(let ((elem (pick s)))
		  (and ((contains rel) (make-op elem elem))
			   (reflexive? (difference 
							rel
							((subrelation/1st rel) elem)))))))))

(reflexive? (make-relation '(1 1) '(2 2) '(1 2) '(3 3)))

'8.18
(define symmetric?
  (lambda (rel)
	((set-equal rel) (inverse-relation rel))))

(symmetric? (make-relation '(1 1) '(2 2) '(1 2) '(2 1)))

'8.19
(define function-compose
  (lambda (f g)
	(let ((rg (range g)) (df (domain f)))
	  (if ((superset df) rg)
		(if ((set-equal g) the-empty-set)
		  the-empty-set
		  (let ((elem1 (pick g)))
			(let ((z (op-2nd elem1)))
			  (let ((elem2 (pick ((subrelation/1st f) z))))
				(adjoin 
				  (make-op (op-1st elem1) (op-2nd elem2))
				  (function-compose f 
									((residue elem1) g)))))))
		(display "composition impossible")))))

(function-compose (make-relation '(1 3) '(2 5) '(10 3))
				  (make-relation '(7 1) '(9 2)))

(function-compose (make-relation '(1 3) '(2 5) '(10 3))
				  (make-relation '(5 9)))


(function-compose (make-relation '(1 3) '(2 5) '(10 3))
				  the-empty-set)

'8.20
(define relation-compose
  (lambda (q r)
	(if ((set-equal q) the-empty-set)
	  the-empty-set
	  (let ((elem1 (pick q)))
		(let ((x (op-1st elem1)) (z (op-2nd elem1)))
		  (union
			(set-map (lambda (op)
					   (make-op x (op-2nd op)))
					 ((subrelation/1st r) z))
			(relation-compose ((residue elem1) q) r)))))))

(relation-compose (make-relation '(1 2) '(3 5) '(2 9))
				  (make-relation '(2 3) '(2 4) '(2 5) '(5 7)))  

'8.21
((subset (make-set 1)) (make-set 3 2))
(define transitive?
  (lambda (rel)
	((subset (relation-compose rel rel)) rel)))

(transitive?
  (make-relation '(1 2) '(1 3) '(1 4) '(2 3) '(2 4) '(3 4)))

(transitive?
  (make-relation '(0 0) '(1 1) '(2 2) '(3 3) '(4 4)))

(transitive?
  (make-relation '(1 1) '(1 2) '(3 2) '(2 1)))

'8.22
(define equivalence-relation?
  (lambda (rel)
	(and (reflexive? rel) (symmetric? rel) (transitive? rel))))

(equivalence-relation?
(make-relation '(0 0) '(1 1) '(2 2) '(3 3)))

(equivalence-relation?
(make-relation '(0 0) '(0 1) '(1 0) '(1 1)))

(equivalence-relation?
(make-relation '(0 0) '(0 1) '(1 1) '(2 2)))
