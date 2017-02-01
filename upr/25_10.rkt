(define (product-simple a b term next)
  (if (> a b)
      1
      (* (term a) (product (next a) b term next))))

(define (sum-simple a b term next)
  (if (> a b)
      0
      (+ (term a) (product (next a) b term next))))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (sum a b term next)
  (accumulate + 0 a b term next))

(define (product a b term next)
  (accumulate * 1 a b term next))

(define (id x ) x)
(define (++ x) (+ x 1))
  
(define (fact-accum n)
  (product 1 n id ++))

(define (expt-accum x n)
  (define (getX k) x)
  (product 1 n getX ++))

(define (count-divisors n a b)
  (define (counter i)
    (if (= 0 (remainder n i)) 1 0))
  (sum a b counter ++))

(define (powers-sum x n)
  (define (term i) (* i (expt x i)))
  (if (and (integer? n) (>= n 0))
      (sum 1 n term ++)
      #f))

(define (integrate f a b)
  (define h 0.001)
  (define (inc i) (+ i h))
  (define (ff a) (* (f a) h))
  (sum a (- b h) ff inc))

(define (combinations n k)
  (define (term i) (/ (- (+ n 1) i) i))
  (define (term2 i) (/ (- (+ n 1) k) i))
  (product 1 k term ++))

(define (f x y) (* x y))
(define f (lambda (x y) (* x y)))

(define (integrate f a b)
  (define g 0.001)
  (sum a (- b h)
       (lambda (a) (* (f a) h))
       (lambda (i) (+ i h))))

(define (derivative f)
  (define h 0.00001)
  (lambda (x) (/ (- (f (+ x h)) (f x)) h)))

(define id-prim (derivative id))

(define (constantly c) (lambda (x) c))

(define (forever-21) (constantly 21))

(define (flip f) (lambda (x y) (f y x)))

(define (compose f g) (lambda (x) (f (g x))))

(define (complement p?)
  (lambda (x) (not (p? x))))

(define (my-even? x) (= 0 (remainder x 2)))
(define my-odd? (complement my-even?))