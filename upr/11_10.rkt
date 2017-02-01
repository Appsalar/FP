
(define (if2 x y z)
  (or (and x y) z))

(if2 (< 1 0) 1 2)

(define (and2 x y)
  (if x y #f))

(define (roots a b c)
  (define d (- (* b b) (* 4 a c)))
  (cond ((and ( = a 0) ( = b 0)) 0)
        ((= a 0) 1)
        ((>  d 0 ) 2)
        ((= d 0) 1)
        (else 0)))

(define (nchoosek n k)
  (if (or (= k 0) (= n k))
      1
      (+ (nchoosek (- n 1) k)
         (nchoosek (- n 1) ( - k 1)))))

(define (nchoosek2 n k)
  (/ (fact n) (* (fact k) (fact (- n k)))))

(define (sq x)
  (* x x))

(define (fast-exp x n)
  (define k (quotient n 2))
  (cond ((zero? n) 1)
      ((even? n) (sq (fast-exp x k)))
      (else (* (sq (fast-exp x k))x))))

(define (fast-exp2 x n)
  (cond ((and (positive? n) (integer? n)) (fast-exp x n))
        ((integer? n) (/ 1(fast-exp x (- n))))
        (else (* (fast-exp2 x (round n))
                 (expt x (- n (round n)))))))