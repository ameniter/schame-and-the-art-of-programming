(load "9.2.scm")

(define num-cols
  (lambda (mat)
	(let ((size (1- (vector-length mat))))
	  (vector-ref mat size))))

(define num-rows
  (lambda (mat)
	(let ((size (1- (vector-length mat))))
	  (/ size (vector-ref mat size)))))

(define matrix-ref
  (lambda (mat)
	(let ((ncols (num-cols mat)))
	  (lambda (i j)
		(vector-ref mat (+ (* i ncols) j))))))

'9.7
(define matrix-ref
  (lambda (mat)
	(let ((ncols (num-cols mat)) (nrows (num-rows mat)))
	  (lambda (i j)
		(if (or (> i nrows) (= i nrows)
				(> j ncols) (= j ncols))
		  (display "Out of range")
		  (vector-ref mat (+ (* i ncols) j)))))))

(let ((A (vector 5 2 3 7 1 4 0 5 8 3 1 2 4)))
  ((matrix-ref A) 1 0))

(define matrix-generator
  (lambda (gen-proc)
	(lambda (nrows ncols)
	  (let ((size (* nrows ncols)))
		(let ((vec-gen-proc
				(lambda (k)
				  (if (< k size)
					(gen-proc (quotient k ncols)
							  (remainder k ncols))
					ncols))))
		  ((vector-generator vec-gen-proc)
		   (1+ size)))))))

(define make-zero-matrix (matrix-generator (lambda (i j) 0)))
(make-zero-matrix 3 5)

(define row-of
  (lambda (mat)
	(let ((mat-ref (matrix-ref mat))
		  (number-of-columns (num-cols mat)))
	  (lambda (i)
		(let ((gen-proc (lambda (j) (mat-ref i j))))
		  ((vector-generator gen-proc) number-of-columns))))))

(define column-of
  (lambda (mat)
	(let ((mat-ref (matrix-ref mat))
		  (number-of-rows (num-rows mat)))
	  (lambda (j)
		(let ((gen-proc (lambda (i) (mat-ref i j))))
		  ((vector-generator gen-proc) number-of-rows))))))

(define matrix-transpose
  (lambda (mat)
	(let ((mat-ref (matrix-ref mat)))
	  (let ((gen-proc (lambda (i j) (mat-ref j i))))
		((matrix-generator gen-proc)
		 (num-cols mat)
		 (num-rows mat))))))

(definGe matrix-product
  (lambda (mat-a mat-b)
	(let ((ncols-a (num-cols mat-a))
		  (a-ref (matrix-ref mat-a))
		  (b-ref (matrix-ref mat-b)))
	  (if (not (= ncols-a (num-rows mat-b)))
		(error "matrix-product:"
			   "The matrices are not compatible.")
		(let
		  ((gen-proc
			 (lambda (i j)
				 (letrec
				   ((loop
					  (lambda (r acc)
						(if (= r ncols-a)
						  acc
						  (loop (1+ r)
								(+ acc (* (a-ref i r)
										  (b-ref r j))))))))
				   (loop 0 0)))))
		  ((matrix-generator gen-proc)
		   (num-rows mat-a) (num-cols mat-b)))))))

(let ((A (vector 4 2 2 5 2 2 4 3 1 3 5 2 3))
	  (B (vector 8 7 4 5 5 4 2)))
	  (matrix-product A B))

(define matrix-set!
  (lambda (mat)
	(let ((ncols (num-cols mat)))
	  (lambda (i j obj)
		(vector-set! mat (+ (* i ncols) j) obj)))))

(let ((A (vector 4 2 2 5 2 2 4 3 1 3 5 2 3)))
  ((matrix-set! A) 0 0 3)
  A)

'9.8
(define matrix
  (lambda (m n)
	(lambda args
	  (if (not (= (length args) (* m n)))
		(error "Incorrect number of inputs")
		((matrix-generator 
		   (lambda (i j)
			 (list-ref args (+ (* i n) j))))
		   m n)))))

((matrix 3 4) 5 2 3 7 
			  1 4 0 5
			  8 3 1 2)

'9.9
(define mat+
  (lambda (A B)
	((matrix-generator
	   (lambda (i j)
		 (+ ((matrix-ref A) i j) ((matrix-ref B) i j))))
	 (num-rows A) (num-cols A))))

(mat+
  ((matrix 2 2) 1 1
				1 1)
  ((matrix 2 2) 2 2
				2 2))

'9.1O
(define matrix-multiply-by-scalar
  (lambda (c A)
	((matrix-generator
	   (lambda (i j)
		 (*  c ((matrix-ref A) i j))))
	 (num-rows A) (num-cols A))))

(matrix-multiply-by-scalar
  2
  ((matrix 2 3) 1 2 3
				4 5 6))

'9.11
(define matrix-view
  (lambda (mat)
	(let ((m (num-rows mat)) (n (num-cols mat)) (mat-ref (matrix-ref mat)))
	  (letrec ((loop
				 (lambda (i j)
				   (if (< i m)
					 (if (< j n)
					   (begin
						 (display (mat-ref i j))
						 (display " ")
						 (loop i (1+ j)))
					   (begin
						 (newline)
						 (loop (1+ i) 0)))))))
		(begin
		  (newline)
		  (loop 0 0))))))

(matrix-view
  ((matrix 2 2) 1 2
				3 4))

'9.13
(define num-cols
  (lambda (mat)
	(let ((size (vector-length mat)))
	  (if (> size 0)
		(vector-length (vector-ref mat 0))
		0))))

(num-cols (vector (vector 2 1 -3) (vector 4 -2 -1)))

(define num-rows vector-length)

(num-rows (vector (vector 2 1 -3) (vector 4 -2 -1)))

(define matrix-ref
  (lambda (mat)
	(lambda (i j)
	  (vector-ref (vector-ref mat i) j))))

((matrix-ref (vector (vector 2 1 -3) (vector 4 -2 -1))) 0 2)

(define matrix-set!
  (lambda (mat)
	(lambda (i j obj)
	  (vector-set! (vector-ref mat i) j obj))))

(let ((mat (vector (vector 2 1 -3) (vector 4 -2 -1))))
  ((matrix-set! mat) 0 2 8)
  mat)


(define matrix-generator
  (lambda (gen-proc)
	(lambda (nrows ncols)
	  (let ((mat-row
			  (lambda (i)
				((vector-generator
				   (lambda (j) 
					 (gen-proc i j))) 
				 ncols))))
		((vector-generator mat-row) nrows)))))

((matrix-generator (lambda (i j) i)) 3 2)
