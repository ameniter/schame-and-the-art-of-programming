;k = jq + r
;gcd(r,j) = rx1 + jy1
;gcd(j,k) = gcd(r,j)
;         = (k - jq)x1+ jy1
;         = kx1 + j(y1- qx1)
;         = j(y1- qx1) + kx1

;gcd(0,m) = m
;j <= k, gcd(j,k) = (d, x, y) , d = jx + ky

(define gcd_x
  (lambda (j k)
	(let ((r (remainder k j)) (q (quotient k j)))
	  (if (= r 0)
		(list j 1 0)
		(let ((l (gcd_x r j)))
		  (let ((d (car l)) (x1 (cadr l)) (y1 (caddr l)))
			(list d (- y1 (* q x1)) x1)))))))

(gcd_x 16 103)
