(define v1 (vector 0 2 4 6 8))
v1
(vector-set! v1 2 5)
v1

(define vector-generator
  (lambda (gen-proc)
	(lambda (size)
	  (let ((vec (make-vector size)))
		(letrec
		  ((loop (lambda (i)
				   (if (< i size)
					 (begin
					   (vector-set!  vec i (gen-proc i))
					   (loop (1+ i)))))))
		  (loop 0))
		vec))))

;((vector-generator (lambda (i) (* 2 i))) 3) 

(define vector-update!
  (lambda (vec i c)
	(vector-set! vec i c)
	vec))

;(vector-update! ((vector-generator (lambda (i) (* 2 i))) 3) 2 7)

(define list->vector
  (lambda (ls)
	(let ((vec (make-vector (length ls))))
	  (letrec
		((convert
		   (lambda (ls i)
			 (if (not (null? ls))
			   (begin
				 (vector-set! vec i (car ls))
				 (convert (cdr ls) (1+ i)))))))
		(convert ls 0))
	  vec)))

(list->vector '(1 3 5))

(define vector-reverse!
  (lambda (vec)
	(let ((swapv! (swap-maker vec)))
	  (letrec
		((switch (lambda (i j)
				   (if (< i j)
					 (begin
					   (swapv! i j)
					   (switch (1+ i) (1- j)))))))
		(switch 0 (1- (vector-length vec))))
	  vec)))

(define swap-maker
  (lambda (vec)
	(lambda (index1 index2)
	  (let ((temp (vector-ref vec index1)))
		(vector-update!
		  (vector-update! vec index1 (vector-ref vec index2))
		  index2
		  temp)))))

(vector-reverse! (vector 10 20 30))

'9.1
(define successive-powers
  (lambda (base)
	(lambda (n)
	  ((vector-generator (lambda (i)
						  (expt base i)))
	   n))))

((successive-powers 2) 8)
((successive-powers 3) 5)


(let ((vec (make-vector 4))) ;n
  (letrec ((loop
			 (lambda (i)
			   (if (< i 4) ;< i n
				 (begin
					 (vector-update! vec i (* 2 (vector-ref vec (1- i))))
					 (loop (1+ i)))))))
	(begin 
	  (vector-update! vec 0 1)
	  (loop 1)))
  vec)

(define sucessive-powers
  (lambda (base)
	(lambda (n)
	  (let ((vec (make-vector n)))
		(letrec ((loop
				   (lambda (i)
					 (if (< i n)
					   (begin
						 (vector-update! vec i (* base (vector-ref vec (1- i))))
						 (loop (1+ i)))))))
		  (begin
			(vector-update! vec 0 1)
			(loop 1)))
		vec))))

((sucessive-powers 2) 8)
((sucessive-powers 3) 5)

'9.2
(define view
  (lambda (vec)
	(let ((highest-index (1- (vector-length vec))))
	  (letrec ((loop (lambda (i)
					   (display (vector-ref vec i))
					   (if (< i highest-index)
						 (begin
						   (display " ")
						   (loop (1+ i)))))))
		(display "#(")
		(if (not (= (vector-length vec) 0))
		  (loop 0))
		(display ")")))))

(view (vector 2 3))

'9.3
(define vector-view
  (lambda (vec)
	(let ((highest-index (1- (vector-length vec))))
	  (letrec ((loop (lambda (i)
					   (display (vector-ref vec i))
					   (if (< i highest-index)
						 (begin
						   (display ", ")
						   (loop (1+ i)))))))
		(display "<")
		(if (not (= (vector-length vec) 0))
		  (loop 0))
		(display ">")))))

(vector-view (vector)) 

'9.5
(define vector-linear-search
  (lambda (vec obj)
	(let ((highest-index (1- (vector-length vec))))
	  (letrec ((helper (lambda (i)
					  (if (not (> i highest-index))
						(if (equal? (vector-ref vec i) obj)
						  i
						  (helper (1+ i)))
						-1))))
		(helper 0)))))

(vector-linear-search '#(g n p r a d l b s) 'a)
(vector-linear-search '#(29 13 96 -5 24 11 9 -15 0 2) 11)

'9.6
(define reverse
  (lambda (vec)
	(let ((length (vector-length vec)))
	  ((vector-generator
		(lambda (i)
		  (vector-ref vec (- (1- length) i))))
	   length))))

(reverse (vector 1 2 3))

(define append
  (lambda (vec1 vec2)
	(let ((length1 (vector-length vec1))
		  (length2 (vector-length vec2)))
	  ((vector-generator
		 (lambda (i)
		   (if (< i length1)
			 (vector-ref vec1 i)
			 (vector-ref vec2 (- i length1)))))
		 (+ length1 length2)))))

(append (vector 1 2 3) (vector 4 5 6))
