(define (add3)
  (lambda (x) (+ x 3)))

(define (flip f)
  (lambda (x y) (f y x)))

(define (curry-bin f)
  (lambda (x)
    (lambda (y) (f x y))))

(define (curry f)
  (lambda (x)
    (lambda (y)
      (lambda (z) (f x y z))))) ; so lambda much wow

(define (prob-odd a b)
  (if (> a b)
      1
      (* ((lambda (x)
            (if (odd? x) x 1)) a)
          (prob-odd (+ a 1) b))))

(define (accumulate op nv term next a b)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv term next (next a) b))))

(define (++ a) (+ a 1))
(define (sq a) (* a a))
(define (term a) (if (odd? a) (sq a) 1))

(define (prod-odd-sq a b)
  (accumulate * 1 term ++ a b))

(define (acc-iter op nv term next a b)
  (define (helper i res)
    (if (> i b)
        res
        (helper (next i) (op (term i) res))))
  (helper a nv))

(define (prod-od-sq-iter a b)
  (acc-iter * 1 term ++ a b))

(define (id x) x)

(define (pow x n)
  (define (const a) x)
  (acc-iter * 1 const ++ 1 n))

(define (fact-dual n)
  (acc-iter * 1 id
            (lambda (x) (+ x 2))
            (if (even? n) 2 1)
            n))

(define (sum x n)
  (acc-iter + 0
            (lambda (i) (/ (expt x i) (+ x i)))
            ++
            1
            n))

(define (prime? n)
  (if (= n 1)
      #f
      (accumulate
       (lambda (x y) (and x y))
       #t
       (lambda (i) (not (= 0 (remainder n i))))
       ++
       2
       (sqrt n))))