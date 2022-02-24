(define remove
  (lambda (item ls)
	(cond
	  ((null? ls) '())
	  ((equal? (car ls) item) (remove item (cdr ls)))
	  (else (cons (car ls) (remove item (cdr ls)))))))

(define set-tag "set")

(define the-empty-set (cons set-tag '()))

(define empty-set?
  (lambda (s)
	(eq? s the-empty-set)))

(define set?
  (lambda (arg)
	(and (pair? arg) (eq? (car arg) set-tag))))

(define pick
  (lambda (s)
	(let ((ls (cdr s)))
	  (if (null? ls)
		(display "error pick: The set is empty.")
		(list-ref ls (random (length ls)))))))

;
;(define residue
;  (lambda (elem)
;	(lambda (s)
;	  (let ((ls (remove elem (cdr s))))
;		(cond 
;		  ((null? ls) the-empty-set)
;		  (else (cons set-tag ls)))))))

;(define adjoin
;  (lambda (elem s)
;	(cons set-tag (cons elem (cdr s)))))

(define remove-1st
  (lambda (item ls)
	(cond
	  ((null? ls) '())
	  ((equal? (car ls) item) (cdr ls))
	  (else (cons (car ls) (remove-1st item (cdr ls)))))))

(define residue
  (lambda (elem)
	(lambda (s)
	  (let ((ls (remove-1st elem (cdr s))))
		(cond
		  ((null? ls) the-empty-set)
		  (else (cons set-tag ls)))))))

(define member? 
  (lambda (item ls)
	(cond
	  ((null? ls) #f)
	  (else (or (equal? (car ls) item)
				(member? item (cdr ls)))))))

(define adjoin
  (lambda (elem s)
	(cond
	  ((member? elem (cdr s)) s)
	  (else (cons set-tag (cons elem (cdr s)))))))
